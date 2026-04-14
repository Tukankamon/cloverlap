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

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { hover : Bool
  , files : List File
  , result : Maybe (List String) -- Result back from backend
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Model False [] Nothing, Cmd.none)

type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFiles File (List File)
  | FileRead String -- Load file to memory
  | ServerResponded (Result Http.Error (List String))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
      ( model
      , Select.files ["text/csv"] GotFiles
      )

    DragEnter ->
      ( { model | hover = True }
      , Cmd.none
      )

    DragLeave ->
      ( { model | hover = False }
      , Cmd.none
      )

    GotFiles file files ->
      ( { model
            | files = file :: files
            , hover = False
        }
      , Task.perform FileRead (File.toString file)
      )
    FileRead contents ->
      ( model
      , Http.post
          { url = "http://localhost:8080/api/optimize"
          , body = Http.stringBody "text/csv" contents
          , expect = Http.expectJson ServerResponded stringListDecoder
          }
      )
    ServerResponded (Ok response) ->
      ( { model | result = Just response }, Cmd.none )

    ServerResponded (Err _) ->
      ( { model | result = Just ["Error talking to server"] }, Cmd.none )


-- Has to be here bc of the browser import
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

uploadDiv : Model -> Html Msg
uploadDiv model =
  div
    [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
    , style "border-radius" "20px"
    , style "width" "280px"
    , style "height" "100px"
    , style "margin" "100px auto"
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

view : Model -> Html Msg
view model = div
  [ style "display" "flex"
  , style "flex-direction" "column"
  --, style "justify-content" "center"
  , style "align-items" "center"
  , style "min-height" "100vh"
  ]
  [
    h1 [] [text "Cloverlap"],
    uploadDiv model,
    h2 [] [
      text (String.join ", " (Maybe.withDefault [] model.result))
    ]
  ]


dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)


-- Stops default browser behaviour of opening the file
hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)


hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)
