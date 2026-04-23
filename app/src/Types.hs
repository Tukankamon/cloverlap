{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Maybe
import Data.Sort
import Data.Time
import GHC.Generics (Generic)

type Blocks = [Maybe TimeBlock]

-- Consider adding Maybes to the other timeblocks aswell
data Course = Course
 { name :: !String
 , semester :: !Int
 , times :: !Blocks
 , exams :: !Blocks
 , skip_class :: !Bool
 , priority :: !Integer
 }
 deriving (Show, Ord)

instance ToJSON Course where
 toJSON c =
  object
   [ "name" .= name c
   , "semester" .= semester c
   , "times" .= times c
   , "exams" .= exams c
   , "skip_class" .= skip_class c
   , "priority" .= priority c
   ]

-- Just check names
instance Eq Course where
 course1 == course2 = name course1 == name course2

type Schedule = [Course]

data TimeBlock = TimeBlock
 { weekday :: Maybe DayOfWeek
 , day :: Maybe Day
 , startTime :: TimeOfDay
 , endTime :: TimeOfDay
 }
 deriving (Eq, Show, Generic)

instance Ord TimeBlock where
 (TimeBlock _ _ start1 end1) `compare` (TimeBlock _ _ start2 end2) =
  (start1, end1) `compare` (start2, end2)

instance ToJSON TimeBlock where
 toJSON (TimeBlock wd d st et) =
  object
   [ "weekday" .= fmap show wd
   , "day" .= fmap show d
   , "startTime" .= show st
   , "endTime" .= show et
   ]

-- #TODO find alternative for "String"
getBlocksFromCourse :: Course -> String -> [TimeBlock]
getBlocksFromCourse course "times" = catMaybes $ times course
getBlocksFromCourse course "exams" = catMaybes $ exams course

-- Unused, delete when necesarry
getDaysFromCourse :: Course -> [DayOfWeek]
getDaysFromCourse course = catMaybes [weekday block | block <- getBlocksFromCourse course "times"]

getNamesFromSchedule :: Schedule -> [String]
getNamesFromSchedule xs = map name xs

getCoursesFromDay :: DayOfWeek -> Schedule -> [Course]
getCoursesFromDay _day list =
 [course | course <- list, Just _day `elem` map weekday (getBlocksFromCourse course "times")]

getDaySchedule :: DayOfWeek -> Schedule -> [TimeBlock]
getDaySchedule _day list = sort $
  [ block
  | course <- list
  , block <- getBlocksFromCourse course "times"
  , Just _day == weekday block
  ]

getWeekSchedule :: Schedule -> [[TimeBlock]]
getWeekSchedule schedule = [getDaySchedule _day schedule | _day <- [Monday .. Sunday]]

-- #TODO make it so it is impossible for a timeblock to be both
isExam :: TimeBlock -> Maybe Bool
isExam (TimeBlock _weekday _day _ _)
 | isNothing _weekday && isJust _day = Just True
 | isJust _weekday && isNothing _day = Just False
 | otherwise = Nothing

data Args = Args
 { input :: String
 , verbose :: Bool
 , classRest :: Integer
 , examRest :: Integer
 , maxClasses :: Integer
 , minClasses :: Integer
 , trimester :: Int -- so as not to overlap with semester
 , loosen :: Bool
 }
 deriving (Show)

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = drop 1 }

-- Some params dont make sense over http so they are removed
data ArgsInput = ArgsInput
 { _classRest :: Integer
 , _examRest :: Integer
 , _maxClasses :: Integer
 , _minClasses :: Integer
 , _trimester :: Int
 , _loosen :: Bool
 } deriving (Show, Generic)

instance FromJSON ArgsInput where
  parseJSON = genericParseJSON customOptions

instance ToJSON ArgsInput where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

inputToArgs :: ArgsInput -> Args
inputToArgs a = Args
  { input = ""
  , verbose = False
  , classRest = _classRest a
  , examRest = _examRest a
  , minClasses = _minClasses a
  , maxClasses = _maxClasses a
  , trimester = _trimester a
  , loosen = _loosen a
  }

