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
import Json.Encode as E
import File
import Task
import Http
import String

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

-- Less fields than in haskell since some dont make sense here like input path
type alias Args =
    { classRest : Int
    , examRest : Int
    , maxClasses : Int
    , minClasses : Int
    , trimester : Int
    , loosen : Bool
    }

defaultArgs : Args
defaultArgs =
    { classRest = 10
    , examRest = 1
    , maxClasses = 8
    , minClasses = 5
    , trimester = 1
    , loosen = False
    }

encodeArgs : Args -> E.Value
encodeArgs args =
    E.object
        [ ( "classRest", E.int args.classRest )
        , ( "examRest", E.int args.examRest )
        , ( "maxClasses", E.int args.maxClasses )
        , ( "minClasses", E.int args.minClasses )
        , ( "trimester", E.int args.trimester )
        , ( "loosen", E.bool args.loosen )
        ]

type alias Model =
  { hover : Bool
  , file : Maybe File
  , result : Maybe (List String) -- Result back from backend, First element is amount of results not shown
  , args : Args
  }

init : () -> (Model, Cmd Msg)
init _ = (Model False Nothing Nothing defaultArgs, Cmd.none)

type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFile File (List File)
  | Clear
  | TrySend
  | FileRead String -- Load file to memory
  | ServerResponded (Result Http.Error (List String))
  -- Arg setting
  | SetClassRest Int
  | SetExamRest Int
  | SetMax Int
  | SetMin Int
  | SetTrimester Int
  | ToggleLoosen Bool

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
        , body =
          Http.jsonBody
            (E.object
              [ ("csv", E.string contents)
              , ("args", encodeArgs model.args)
              ]
            )
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
      ( { model | result = Just [ "Error talking to server " ++ message] }, Cmd.none )

    SetClassRest n ->
      (updateArgs (\a -> { a | classRest = n}) model , Cmd.none)

    SetExamRest n ->
      (updateArgs (\a -> { a | examRest = n}) model , Cmd.none)

    SetMax n ->
      (updateArgs (\a -> { a | maxClasses = n}) model , Cmd.none)

    SetMin n ->
      (updateArgs (\a -> { a | minClasses = n}) model , Cmd.none)

    SetTrimester n ->
      (updateArgs (\a -> { a | trimester = n}) model , Cmd.none)

    ToggleLoosen b ->
      (updateArgs (\a -> { a | loosen = b}) model , Cmd.none)

updateArgs : ( Args -> Args ) -> Model -> Model 
updateArgs fn m = 
  {m | args = fn m.args }

-- Has to be here bc of the browser import
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- #TODO have this preview the csv once it is uploaded
uploadDiv : Model -> Html Msg
uploadDiv model = div
  [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
  , style "border-radius" "20px"
  , style "width" "280px"
  , style "height" "150px"
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
  [
  if (model.file == Nothing) then
    button [ onClick Pick ] [ text "Upload CSV file" ]
  else argsGrid model
  --, span [ style "color" "#ccc" ] [ text (Debug.toString model) ]
  ]

parseInt : String -> Int
parseInt s = Maybe.withDefault 0 (String.toInt s)

parseBool : String -> Bool
parseBool s = case s of
  "true" -> True
  "false" -> False
  _ -> False

inputBox : String -> String -> (String -> msg) -> Html msg
inputBox title v toMsg = div
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "justify-content" "center"
  ]
  [ label [] [ text title]
  , input
    [ placeholder title
    , value v
    , onInput toMsg
    , style "width" "60px"
    ]
    []
  ]

-- Grid to select args on the request
argsGrid : Model -> Html Msg
argsGrid model = div
  [ style "display" "grid"
  , style "grid-template-columns" "1fr 1fr"
  , style "gap" "5px"
  , style "width" "200px"
  ]
  [ inputBox "Class rest" (String.fromInt model.args.classRest) (\s -> SetClassRest (parseInt s))
  , inputBox "Exam rest" (String.fromInt model.args.examRest) (\s -> SetExamRest (parseInt s))
  , inputBox "Max classes" (String.fromInt model.args.maxClasses) (\s -> SetMax (parseInt s))
  , inputBox "Min classes" (String.fromInt model.args.minClasses) (\s -> SetMin (parseInt s))
  , inputBox "Trimester" (String.fromInt model.args.trimester) (\s -> SetTrimester (parseInt s))
  , label [] [ text "Loosen", input [ type_ "checkbox", checked model.args.loosen, onCheck ToggleLoosen] [] ]
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

-- Just for the function below
headerText : String -> Html Msg
headerText n = case n of
  "0" -> text "Only one optimal configuration of classes was found with your requirements:"
  _ -> text ("The following classes are the most optimal for your requirements with " ++ n ++ " other configurations possible:")

-- #TODO make result prettier
showResult : Model -> Html Msg
showResult model = case model.result of
  Just [x] -> h2 [] [ text x ] -- Errors are shown like this
  Just (x::xs) -> div []
    [ h2 [] [ headerText x ]
    , ul
      [ style "display" "flex"
      , style "flex-direction" "column"
      --, style "align-items" "center"
      ]
      (List.map (\elem -> li [] [ text elem ]) xs)
    ]
  _ -> text ""

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
