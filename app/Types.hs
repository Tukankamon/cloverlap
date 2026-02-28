module Types where

import Data.Time
import Data.Maybe (isJust, isNothing)

-- Consider adding Maybes to the other timeblocks aswell
data Course = Course {
  name :: !String,
  time1 :: !TimeBlock, time2 :: !TimeBlock, time3 :: Maybe TimeBlock,
  exam1 :: !TimeBlock, exam2 :: !TimeBlock, exam3 :: Maybe TimeBlock,
  skip_class :: !Bool,
  priority :: !Int
} deriving Show

-- Just check names
instance Eq Course where
  course1 == course2 = name course1 == name course2

data TimeBlock = TimeBlock {
  weekday :: Maybe DayOfWeek,
  day :: Maybe Day,
  startTime :: TimeOfDay,
  endTime :: TimeOfDay
} deriving (Eq, Show)

isExam :: TimeBlock -> Maybe Bool
isExam (TimeBlock _weekday _day _ _)
  | isNothing _weekday && isJust _day = Just True
  | isJust _weekday && isNothing _day = Just False
  | otherwise = Nothing
