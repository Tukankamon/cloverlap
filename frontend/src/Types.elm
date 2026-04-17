module Types exposing (..)

import File exposing (File)
import Http

type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFile File (List File)
  | Clear
  | TrySend
  | FileRead String -- Load file to memory
  | ServerResponded (Result Http.Error Response)
  -- Arg setting
  | SetClassRest Int
  | SetExamRest Int
  | SetMax Int
  | SetMin Int
  | SetTrimester Int
  | ToggleLoosen Bool

type alias Model =
  { hover : Bool
  , file : Maybe File
  , response : Response
  , args : Args
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

type alias Response =
  { title : String
  , classes : List String
  --#TODO Not actually string, need to implement Day and TimeBlock types here
  , calendar : List (List String)
  , exams : List String
  }

emptyResponse : Response
emptyResponse = Response "" [] [] []

type Day
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

week : List Day
week = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

dayToString : Day -> String
dayToString day = case day of
  Mon -> "Monday"
  Tue -> "Tuesday"
  Wed -> "Wednesday"
  Thu -> "Thursday"
  Fri -> "Friday"
  Sat -> "Saturday"
  Sun -> "Sunday"
