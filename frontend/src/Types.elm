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
    { classRest = 0
    , examRest = 0
    , maxClasses = 8
    , minClasses = 5
    , trimester = 1
    , loosen = False
    }

type alias Schedule = List Course
type alias Course =
  { name : String
  , semester : Int
  , times : Blocks
  , exams : Blocks
  , skip_class : Bool
  , priority : Int
  }

getClasses : Schedule -> List String
getClasses list = List.map .name list

type alias TimeOfDay =
  { hour : Int --#TODO find better type to represent this
  , minute : Int
  }

timeToSecs : TimeOfDay -> Int
timeToSecs t = (60 * t.hour) + t.minute

-- Read comment on the haskell equivalent
getBlocksFromCourse : Course -> String -> List TimeBlock
getBlocksFromCourse course key = case key of
  "times" -> course.times
  "exams" -> course.exams
  _ -> []

type alias Blocks = List TimeBlock
type alias TimeBlock =
  { name : String
  , weekday : Maybe Day
  , day : Maybe String -- Exam date, unimplemented for now
  , startTime : TimeOfDay
  , endTime : TimeOfDay
  }

oclock : Int -> TimeOfDay
oclock time = {hour = time, minute = 0}

noon : TimeOfDay
noon = oclock 12

testBlock : TimeBlock
testBlock =
  {name = "test", weekday = Just Mon, day = Nothing, startTime = noon, endTime = oclock 13}

type alias Response =
  { title : String
  , calendar : Schedule
  --#TODO Not actually string, need to implement Day and TimeBlock types here
  , tests : List String
  }

getDaySchedule : Day -> Schedule -> Blocks
getDaySchedule day schedule = schedule
  |> List.concatMap (\course -> 
    getBlocksFromCourse course "times"
      |> List.map (\block -> { block | name = course.name }))
  |> List.filter (\block -> block.weekday == Just day)
  |> List.sortBy (\b -> timeToSecs b.startTime)

allExamBlocks : Schedule -> List TimeBlock
allExamBlocks schedule = schedule
  |> List.concatMap (\course ->
      getBlocksFromCourse course "exams"
        |> List.map (\block -> { block | name = course.name }))

dedupe : List String -> List String
dedupe list = case list of
  [] -> []
  x :: rest ->
    x :: dedupe (List.filter ((/=) x) rest)

-- #TODO sort the result
getExamDates : Schedule -> List String
getExamDates schedule = allExamBlocks schedule
  |> List.filterMap .day
  |> dedupe
  |> List.sort

getExamSchedule : String -> Schedule -> Blocks
getExamSchedule dateStr schedule = allExamBlocks schedule
    |> List.filter (\block -> block.day == Just dateStr)
    |> List.sortBy (\b -> timeToSecs b.startTime)

emptyResponse : Response
emptyResponse = Response "" [] []

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


stringToDay : String -> Maybe Day
stringToDay day = case day of
  "Monday" -> Just Mon
  "Tuesday" -> Just Tue
  "Wednesday" -> Just Wed
  "Thursday" -> Just Thu
  "Friday" -> Just Fri
  "Saturday" -> Just Sat
  "Sunday" -> Just Sun
  _ -> Nothing

numToMonth : String -> String
numToMonth s = case s of
  "01" -> "January"
  "02" -> "February"
  "06" -> "June"
  "07" -> "July"
  "10" -> "October"
  _ -> "Other"
