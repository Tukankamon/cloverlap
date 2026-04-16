module Network exposing (..)
-- module for sending, recieving and decoding from Http

import Types exposing (..)

import Json.Decode as D
import Json.Encode as E
import Http
import File exposing (File)
import File.Select as Select

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

sendCsv : String -> Args -> Cmd Msg
sendCsv contents args = Http.post
  { url = "http://localhost:8080/api/optimize"
  , body =
    Http.jsonBody
      (E.object
        [ ( "csv", E.string contents )
        , ( "args", encodeArgs args )
        ]
      )
  , expect = Http.expectStringResponse ServerResponded expectedHttp
  }

expectedHttp : Http.Response String -> Result Http.Error Response
expectedHttp response = case response of
  Http.GoodStatus_ _ body ->
    case D.decodeString responseDecoder body of
      Ok result -> Ok result
      Err err -> Err (Http.BadBody (D.errorToString err))

  Http.BadStatus_ _ body ->
      Err (Http.BadBody body)

  Http.Timeout_ ->
      Err Http.Timeout

  Http.NetworkError_ ->
      Err Http.NetworkError

  Http.BadUrl_ url ->
      Err (Http.BadUrl url)

erroneusResponse : Http.Error -> String
erroneusResponse err = case err of
  Http.Timeout -> "Request timed out"
  Http.NetworkError -> "Network error"
  Http.BadBody body -> body
  Http.BadStatus code -> "Server error: " ++ String.fromInt code
  Http.BadUrl url -> "Bad URL: " ++ url

responseDecoder : D.Decoder Response
responseDecoder =
  D.map4 Response
    (D.field "title" D.string)
    (D.field "classes" (D.list D.string))
    (D.field "calendar" (D.list (D.list D.string)))
    (D.field "exams" (D.list D.string))

