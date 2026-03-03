module Types where

import Data.Time
import Data.Sort
import Data.Maybe

-- Consider adding Maybes to the other timeblocks aswell
data Course = Course {
  name :: !String,
  semester :: !Int,
  time1 :: !TimeBlock, time2 :: !TimeBlock, time3 :: Maybe TimeBlock,
  exam1 :: !TimeBlock, exam2 :: !TimeBlock, exam3 :: Maybe TimeBlock,
  skip_class :: !Bool,
  priority :: !Integer
} deriving Show

-- Just check names
instance Eq Course where
  course1 == course2 = name course1 == name course2

type Schedule = [Course]

data TimeBlock = TimeBlock {
  weekday :: Maybe DayOfWeek,
  day :: Maybe Day,
  startTime :: TimeOfDay,
  endTime :: TimeOfDay
} deriving (Eq, Show)

instance Ord TimeBlock where
  (TimeBlock _ _ start1 end1) `compare` (TimeBlock _ _ start2 end2) =
    (start1, end1) `compare` (start2, end2)

getBlockFromCourse :: Course -> [TimeBlock]
getBlockFromCourse course =
  catMaybes [Just $ time1 course, Just $ time2 course, time3 course]

-- Unused, delete when necesarry
getDaysFromCourse :: Course -> [DayOfWeek]
getDaysFromCourse course =
  catMaybes [weekday block | block<-blocks]
  where
    blocks = [ time1 course, time2 course ] ++ maybeToList (time3 course)

getDaySchedule :: DayOfWeek -> Schedule -> [TimeBlock]
getDaySchedule _day list =
  sort $ [block | course<-list,
  block<-getBlockFromCourse course, Just _day == weekday block ]

-- #TODO make it so it is impossible for a timeblock to be both
isExam :: TimeBlock -> Maybe Bool
isExam (TimeBlock _weekday _day _ _)
  | isNothing _weekday && isJust _day = Just True
  | isJust _weekday && isNothing _day = Just False
  | otherwise = Nothing
