module Main exposing (..)

import Network exposing (..)
import View exposing (view)
import Types exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Task
import Http

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init _ = (Model False Nothing emptyResponse defaultArgs, Cmd.none)

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
      ( { model | file = Nothing, response = emptyResponse }
      , Cmd.none
      )

    TrySend -> case model.file of
      Nothing ->
        ( { model | response =
          Response "No csv to send! Click 'upload CSV file'" [] []
          }
        , Cmd.none )
      Just file ->
        ( model, Task.perform FileRead (File.toString file))

    FileRead contents ->
      ( model, sendCsv contents model.args)

    ServerResponded (Ok r) ->
      ( { model | response = r }, Cmd.none )

    ServerResponded (Err err) ->
      ( { model | response =
        Response ("Error talking to server " ++ erroneusResponse err) [] []
        }
      , Cmd.none )

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
updateArgs fn model = {model | args = fn model.args }

-- Has to be here bc of the browser import
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

