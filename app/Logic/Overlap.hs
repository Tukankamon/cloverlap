module Logic.Overlap (overlapInList) where

import Types
import Data.Time
import Data.Maybe (fromJust)
-- #TODO find a way to make the error handling cleaner

-- #TODO improve clarity, this returns a single list of tuples containing two courses and a list of the TimeBlock overlaps of those two courses
overlapInList :: [Course]->[Integer]->[(Course,Course,[(TimeBlock,TimeBlock)])]
overlapInList list rests =
  removeMirroredPair [(course1, course2, overlaps)
  | course1<-list, course2<-list, course1 /= course2, -- Comparison is by name
  let overlaps = coursesOverlap course1 course2 rests,
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
coursesOverlap :: Course -> Course -> [Integer] -> [(TimeBlock, TimeBlock)]
coursesOverlap course1 course2 rests = _classOverlap ++ _examOverlap
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
      (False, False) -> listsOverlap (classList course1) (classList course2) rests
    _examOverlap = listsOverlap (examList course1) (examList course2) rests

listsOverlap :: [TimeBlock] -> [TimeBlock] -> [Integer] -> [(TimeBlock,TimeBlock)]
listsOverlap list1 list2 rests =
  [ (block1, block2) | block1<-list1, block2<-list2,
  timeBlockOverlap block1 block2 rests == Just True]

timeBlockOverlap :: TimeBlock -> TimeBlock -> [Integer] -> Maybe Bool
timeBlockOverlap _ _ rests
  | length rests /= 4 = Nothing
timeBlockOverlap block1 block2 rests =
  case (isExam block1, isExam block2) of
    (Just True, Just True) -> Just $ examOverlap block1 block2 _examRest
    (Just False, Just False) -> Just $ classOverlap block1 block2 _classRest
    _ -> Nothing
    where
      _classRest = (rests !! 0)
      _examRest = (rests !! 1)

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
examOverlap block1 block2 dayRest =
  abs (diffDays (fromJust $ day block1) (fromJust $ day block2)) < dayRest

