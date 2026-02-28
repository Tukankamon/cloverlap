module Types where

import Data.Time
import Data.Maybe (isJust, isNothing)

data Course = Course {
  name :: !String,
  time1 :: !TimeBlock, time2 :: !TimeBlock, time3 :: !TimeBlock,
  exam1 :: !TimeBlock, exam2 :: !TimeBlock,
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

-- Must check that weekday and day are not set at the same time
instance Ord TimeBlock where
  compare (TimeBlock w1 d1 s1 e1) (TimeBlock w2 d2 s2 e2) = 
    compare (d1, w1, s1, e1) (d2, w2, s2, e2)

isExam :: TimeBlock -> Maybe Bool
isExam (TimeBlock _weekday _day _ _)
  | isNothing _weekday && isJust _day = Just True
  | isJust _weekday && isNothing _day = Just False
  | otherwise = Nothing
