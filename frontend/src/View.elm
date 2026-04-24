module View exposing (view)

import Types exposing (..)

import Json.Decode as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import File exposing (File)
import String

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
  --#TODO add button to send next-best schedule
  ]

headerText : Response -> Html Msg
headerText r = case String.toInt r.title of
  Just 1 -> text "Only one optimal configuration of classes was found with your requirements:"
  Just n -> text ("The following classes are the most optimal for your requirements with " ++ String.fromInt (n - 1) ++ " other configurations possible:")
  Nothing -> text "Error parsing response"

viewCourseItem : Course -> Html Msg
viewCourseItem course =
  if course.skip_class then
    li [ style "color" "red" ]
      [ text (course.name ++ " (not attended)") ]
  else
    li [] [ text course.name ]

-- #TODO make result prettier
showResult : Model -> Html Msg
showResult model = case model.response.calendar of
  [] -> h2 [] [ text model.response.title ] -- Errors are shown like this
  list -> div []
    [ h2 [] [ headerText model.response ]
    --, text (Debug.toString model.response)
    , ul
      [ style "display" "flex"
      , style "flex-direction" "column"
      --, style "align-items" "center"
      ]
      (List.map viewCourseItem list)
    ]

formatBlock : TimeOfDay -> String
formatBlock time =
  let f t = String.padLeft 2 '0' (String.fromInt t) in
  f time.hour ++ ":" ++ f time.minute

viewBlock : TimeBlock -> Html Msg
viewBlock block = div
  [ style "background" "#e8f0fd"
  , style "border-radius" "8px"
  , style "padding" "8px 10px"
  ]
  [ div [style "font-weight" "500", style "font-size" "13px"]
    [ text block.name ]
  , div [style "font-weight" "11px", style "opacity" "0.8", style "margin-top" "2px"]
    [ text ((formatBlock block.startTime) ++ "-" ++ (formatBlock block.endTime))
    ]
  ]

viewDay : Model -> Day -> Html Msg
viewDay model day =
  let
    filteredCalendar = List.filter (\c -> not c.skip_class) model.response.calendar
    schedule = getDaySchedule day filteredCalendar
  in
  div
    [ style "flex" "1"
    , style "background" "var(--color-background-secondary)"
    , style "border-radius" "12px"
    , style "padding" "12px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "gap" "6px"
    ]
    ([ h3 [] [ text (dayToString day) ] ]
    ++ List.map viewBlock schedule)
    

--#TODO fix that depending on week day name length the calendar changes size
viewWeek : Model -> Html Msg
viewWeek model = div
  [ style "display" "flex"
  , style "flex-direction" "row"
  , style "gap" "32px"
  ]
  (week
  -- Inefficient since getDaySchedule happens twice
  |> List.filter (\day -> getDaySchedule day model.response.calendar /= [])
  |> List.map (viewDay model))


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
  , viewWeek model
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
