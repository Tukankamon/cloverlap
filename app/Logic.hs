module Logic (overlapInVector) where

import Types
import Data.Time
import Data.Maybe (fromJust)
import qualified Data.Vector as V
-- #TODO find a way to make the error handling cleaner cuz there are too many fromJusts

-- Returns the pairs that overlap
overlapInVector :: V.Vector Course -> [Integer] -> [(Course, Course)]
overlapInVector v rests =
  removeMirrored [(course1, course2)
  | course1<-list, course2<-list, course1 /= course2,
  coursesOverlap course1 course2 rests == True]
  where
    list = V.toList v

removeMirrored :: Eq a => [(a,a)] -> [(a,a)]
removeMirrored list = filter keep list
  where keep (a,b) = not ((b,a) `elem` list) && a/=b

-- This throws away the list of overlaps for now, might need them later
coursesOverlap :: Course -> Course -> [Integer] -> Bool
coursesOverlap course1 course2 rests
  | _classesDontOverlap || (skip_class course1) || (skip_class course2) = False
  | _examsDontOverlap = False
  | otherwise = True
  where
    classList some_course = [time1 some_course, time2 some_course, time3 some_course]
    examList some_course = [exam1 some_course, exam2 some_course]
    _classesDontOverlap =
      listsOverlap (classList course1) (classList course2) rests == []
    _examsDontOverlap = listsOverlap (examList course1) (examList course2) rests == []

listsOverlap :: [TimeBlock] -> [TimeBlock] -> [Integer] -> [(TimeBlock,TimeBlock)]
listsOverlap list1 list2 rests =
  [ (block1, block2) | block1<-list1, block2<-list2,
  timeBlockOverlap block1 block2 rests == Just True]

timeBlockOverlap :: TimeBlock -> TimeBlock -> [Integer] -> Maybe Bool
timeBlockOverlap _ _ rests
  | length rests /= 2 = Nothing -- rests must be classRest and examRest, no more
timeBlockOverlap block1 block2 rests =
  case (isExam block1, isExam block2) of
    (Just True, Just True) -> Just $ examOverlap block1 block2 (rests !! 0)
    (Just False, Just False) -> Just $ classOverlap block1 block2 (rests !! 1)
    _ -> Nothing

classOverlap :: TimeBlock -> TimeBlock -> Integer -> Bool
classOverlap block1 block2 minRest =
  timeDiff > restSeconds && (weekday block1) == (weekday block2)
  where
    -- Probably too many helpers here
    comesFirst = minimum [block1, block2] -- Defined in Types.hs
    comesLast = maximum [block1, block2]
    firstFinish = endTime comesFirst
    lastStart = startTime comesLast
    restSeconds = fromIntegral (minRest * 60)
    timeDiff = (timeOfDayToTime firstFinish) - (timeOfDayToTime lastStart)

-- #TODO allow for exams on the same day but different hours
examOverlap :: TimeBlock -> TimeBlock -> Integer -> Bool
examOverlap block1 block2 dayRest =
  diffDays (fromJust $ day block1) (fromJust $ day block2) < dayRest
