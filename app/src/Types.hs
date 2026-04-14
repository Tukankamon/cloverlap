{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Maybe
import Data.Sort
import Data.Time

-- Consider adding Maybes to the other timeblocks aswell
data Course = Course
 { name :: !String
 , semester :: !Int
 , time1 :: !TimeBlock
 , time2 :: !TimeBlock
 , time3 :: Maybe TimeBlock
 , exam1 :: !TimeBlock
 , exam2 :: !TimeBlock
 , exam3 :: Maybe TimeBlock
 , skip_class :: !Bool
 , priority :: !Integer
 }
 deriving (Show)

instance ToJSON Course where
 toJSON c =
  object
   [ "name" .= name c
   , "semester" .= semester c
   , "time1" .= time1 c
   , "time2" .= time2 c
   , "time3" .= time3 c
   , "exam1" .= exam1 c
   , "exam2" .= exam2 c
   , "exam3" .= exam3 c
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
 deriving (Eq, Show)

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

getBlockFromCourse :: Course -> [TimeBlock]
getBlockFromCourse course =
 catMaybes [Just $ time1 course, Just $ time2 course, time3 course]

-- Unused, delete when necesarry
getDaysFromCourse :: Course -> [DayOfWeek]
getDaysFromCourse course =
 catMaybes [weekday block | block <- blocks]
 where
 blocks = [time1 course, time2 course] ++ maybeToList (time3 course)

getNamesFromSchedule :: Schedule -> [String]
getNamesFromSchedule [] = []
getNamesFromSchedule (x:xs) = (name x) : getNamesFromSchedule xs

getCoursesFromDay :: DayOfWeek -> Schedule -> [Course]
getCoursesFromDay _day list =
 [course | course <- list, any (== Just _day) (map weekday (getBlockFromCourse course))]

getDaySchedule :: DayOfWeek -> Schedule -> [TimeBlock]
getDaySchedule _day list =
 sort $
  [ block
  | course <- list
  , block <- getBlockFromCourse course
  , Just _day == weekday block
  ]

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
