module Main exposing (..)
-- Image upload with a drag and drop zone.
-- Dependencies:
--   elm install elm/file
--   elm install elm/json

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import File
import Task
import Http

stringListDecoder : D.Decoder (List String)
stringListDecoder = D.list D.string

stringOrListDecoder : D.Decoder (List String)
stringOrListDecoder = D.oneOf
  [ stringListDecoder
  , D.map List.singleton D.string
  ]

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { hover : Bool
  , file : Maybe File
  , result : Maybe (List String) -- Result back from backend
  }

init : () -> (Model, Cmd Msg)
init _ = (Model False Nothing Nothing, Cmd.none)

type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFile File (List File)
  | Clear
  | TrySend
  | FileRead String -- Load file to memory
  | ServerResponded (Result Http.Error (List String))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
      ( model
      , Select.files ["text/csv"] GotFile
      )

    DragEnter ->
      ( { model | hover = True }
      , Cmd.none
      )

    DragLeave ->
      ( { model | hover = False }
      , Cmd.none
      )

    GotFile file _ ->
      ( { model | file = Just file, hover = False }
      --, Task.perform FileRead (File.toString file)
      , Cmd.none
      )

    Clear ->
      ( { model | file = Nothing, result = Nothing }
      , Cmd.none
      )

    TrySend -> case model.file of
      Nothing ->
        ( { model | result = Just ["No csv to send! Click 'upload CSV file'"] }, Cmd.none )
      Just file ->
        ( model, Task.perform FileRead (File.toString file))

    FileRead contents ->
      ( model
      , Http.post
        { url = "http://localhost:8080/api/optimize"
        , body = Http.stringBody "text/csv" contents
        , expect =
          Http.expectStringResponse ServerResponded
            (\response -> case response of
              Http.GoodStatus_ _ body ->
                case D.decodeString stringOrListDecoder body of
                  Ok list -> Ok list
                  Err _ -> Ok [ body ]

              Http.BadStatus_ _ body ->
                  Err (Http.BadBody body)

              Http.Timeout_ ->
                  Err Http.Timeout

              Http.NetworkError_ ->
                  Err Http.NetworkError

              Http.BadUrl_ url ->
                  Err (Http.BadUrl url)
                    )
            }
        )
    ServerResponded (Ok response) ->
      ( { model | result = Just response }, Cmd.none )

    ServerResponded (Err err) ->
      let
        message = case err of
          Http.Timeout -> "Request timed out"
          Http.NetworkError -> "Network error"
          Http.BadBody body -> body
          Http.BadStatus code -> "Server error: " ++ String.fromInt code
          Http.BadUrl url -> "Bad URL: " ++ url
      in
      ( { model | result = Just [ "Error talking to server" ++ message] }, Cmd.none )

-- Has to be here bc of the browser import
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- #TODO have this preview the csv once it is uploaded
uploadDiv : Model -> Html Msg
uploadDiv model = div
  [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
  , style "border-radius" "20px"
  , style "width" "280px"
  , style "height" "100px"
  , style "margin" "80px auto"
  , style "padding" "20px"
  , style "display" "flex"
  , style "flex-direction" "column"
  , style "justify-content" "center"
  , style "align-items" "center"
  , hijackOn "dragenter" (D.succeed DragEnter)
  , hijackOn "dragover" (D.succeed DragEnter)
  , hijackOn "dragleave" (D.succeed DragLeave)
  , hijackOn "drop" dropDecoder
  ]
  [ button [ onClick Pick ] [ text "Upload CSV file" ]
  --, span [ style "color" "#ccc" ] [ text (Debug.toString model) ]
  ]

rowButton : msg -> List (Attribute msg)
rowButton msg =
  [ style "width" "80px"
  , style "height" "40px"
  , style "align-self" "center"
  ] ++ [ onClick msg ]

interactive : Model -> Html Msg
interactive model = div
  [ style "justify-content" "center"
  , style "display" "flex"
  , style "flex-direction" "row"
  , style "gap" "20px"
  ]
  [ button (rowButton Clear) [text "clear"]
  , uploadDiv model
  , button (rowButton TrySend) [text "GO!"]
  ]

-- #TODO make result prettier
showResult : Model -> Html Msg
showResult model = case model.result of
  Just [x] -> h2 [] [ text x ] -- Errors are shown like this
  Just items -> div []
    [ h2 [] [ text "The following classes are the most optimial for your requirements" ]
    , ul
      [ style "display" "flex"
      , style "flex-direction" "column"
      , style "align-items" "center"
      ]
      (List.map (\elem -> li [] [ text elem ]) items)
    ]
  _ -> div [] []

view : Model -> Html Msg
view model = div
  [ style "display" "flex"
  , style "flex-direction" "column"
  --, style "justify-content" "center"
  , style "align-items" "center"
  , style "min-height" "100vh"
  ]
  [ h1 [] [text "Cloverlap"]
  , interactive model
  , showResult model
  ]

dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFile File.decoder)

-- Stops default browser behaviour of opening the file
hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)

hijack : msg -> (msg, Bool)
hijack msg = (msg, True)
