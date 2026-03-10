module Overlap (overlapInList) where

import Types
import Data.Time
-- #TODO find a way to make the error handling cleaner

-- #TODO improve clarity, this returns a single list of tuples containing two courses and a list of the TimeBlock overlaps of those two courses
overlapInList :: [Course]-> Args ->[(Course,Course,[(TimeBlock,TimeBlock)])]
overlapInList list args =
  removeMirroredPair [(course1, course2, overlaps)
  | course1<-list, course2<-list, course1 /= course2, -- Comparison is by name
  let overlaps = coursesOverlap course1 course2 args,
  overlaps /= [] ]

removeMirroredPair :: Eq a => [(a,a,b)] -> [(a,a,b)]
removeMirroredPair triples = go [] triples
  where
    go :: Eq a => [(a,a)] -> [(a,a,b)] -> [(a,a,b)]
    go _ [] = []
    go seen ((x,y,z):rest)
      | x == y = go seen rest
      | (x,y) `elem` seen || (y,x) `elem` seen = go seen rest
      | otherwise = (x,y,z) : go ((x,y):seen) rest

-- #TODO make this function more readable
coursesOverlap :: Course -> Course -> Args -> [(TimeBlock, TimeBlock)]
coursesOverlap course1 course2 args = _classOverlap ++ _examOverlap
  where
    classList some_course =
      case time3 some_course of
        Just tb-> [time1 some_course, time2 some_course, tb]
        Nothing-> [time1 some_course, time2 some_course]

    examList some_course = case exam3 some_course of
      Just tb -> [exam1 some_course, exam2 some_course, tb]
      Nothing -> [exam1 some_course, exam2 some_course]
    _classOverlap = case (skip_class course1, skip_class course2) of
      (True, _) -> []
      (_, True) -> []
      (False, False) -> listsOverlap (classList course1) (classList course2) args
    _examOverlap = listsOverlap (examList course1) (examList course2) args

listsOverlap :: [TimeBlock] -> [TimeBlock] -> Args -> [(TimeBlock,TimeBlock)]
listsOverlap list1 list2 args =
  [ (block1, block2) | block1<-list1, block2<-list2,
  timeBlockOverlap block1 block2 args == Just True]

timeBlockOverlap :: TimeBlock -> TimeBlock -> Args -> Maybe Bool
timeBlockOverlap block1 block2 args =
  case (isExam block1, isExam block2) of
    (Just True, Just True) -> Just $ examOverlap block1 block2 (examRest args)
    (Just False, Just False) -> Just $ classOverlap block1 block2 (classRest args)
    _ -> Nothing

classOverlap :: TimeBlock -> TimeBlock -> Integer -> Bool
classOverlap block1 block2 minRest =
  timeDiff > restSeconds && (weekday block1) == (weekday block2)
  where
    firstFinish, lastStart :: TimeOfDay
    firstFinish = minimum [endTime block1, endTime block2]
    lastStart = maximum [startTime block1, startTime block2]
    restSeconds = fromIntegral (minRest * 60)
    timeDiff = (timeOfDayToTime firstFinish) - (timeOfDayToTime lastStart)

-- #TODO allow for exams on the same day but different hours
examOverlap :: TimeBlock -> TimeBlock -> Integer -> Bool
examOverlap block1 block2 0 = (timeOfDayToTime firstFinish) - (timeOfDayToTime lastStart) < 0
  where
    firstFinish, lastStart :: TimeOfDay
    firstFinish = minimum [endTime block1, endTime block2]
    lastStart = maximum [startTime block1, startTime block2]
    timeDiff = (timeOfDayToTime firstFinish) - (timeOfDayToTime lastStart)
examOverlap block1 block2 dayRest = case (day block1, day block2) of
  (Just d1, Just d2) -> abs (diffDays d1 d2) < dayRest
  _ -> False

