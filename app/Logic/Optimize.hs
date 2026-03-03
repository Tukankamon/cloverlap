module Logic.Optimize (bestSchedule) where

import Types
import Logic.Overlap
import Data.Time
import Data.List

-- #TODO check for multiple possible results
-- #TODO dont be so strict, if the conditions dont return any then loosen them a bit
bestSchedule :: Schedule -> Int -> [Integer] -> Maybe Schedule
bestSchedule set _semester rests
  | list == [] = Nothing
  | length rests /= 4 = Nothing --should be: classRest, examRest, maxClasses, minClasses
  | otherwise = Just $ head $ sortBy downtimeSort list
  where
    list = generateAllCombinations set _semester rests
    -- Add more conditions as needed
    downtimeSort :: Schedule -> Schedule -> Ordering
    downtimeSort s1 s2
      | computePriority s1 < computePriority s2 = GT
      | computePriority s1 > computePriority s2 = LT
      | weekDowntimePerClass s1 <= weekDowntimePerClass s2 = GT
      | otherwise = LT

generateAllCombinations :: Schedule -> Int -> [Integer] -> [Schedule]
generateAllCombinations list _semester rests =
  let
    classRest = rests !! 0
    examRest = rests !! 1
    maxClasses = rests !! 2
    minClasses = rests !! 3
  in
    [ pick | pick <- subsequences list,
    length pick <= fromInteger maxClasses,
    computeAttendance pick >= minClasses,
    -- There is probably a better way to do the following
    [ course | course<-pick, semester course == _semester]  == pick,
    overlapInList pick [classRest,examRest] == []]

computePriority :: Schedule -> Integer
computePriority schedule = sum [priority course | course<-schedule ]

-- Doesnt apply for exams
-- #TODO find better name
computeAttendance :: Schedule -> Integer
computeAttendance schedule =
  toInteger $ length [course | course<-schedule, skip_class course == False ]

-- Does Monday..Sunday do the intended behaviour?
weekDowntimePerClass :: Schedule -> Maybe Double
weekDowntimePerClass [] = Nothing
weekDowntimePerClass list =
  let
    totalDowntime = sum [computeDowntime _day list | _day<-[Monday ..Sunday]]
    numberOfClasses = length list
  in Just (fromIntegral totalDowntime / fromIntegral numberOfClasses)

-- Couldnt find a function to diff time of TimeOfDay
computeDowntime :: DayOfWeek -> Schedule -> Integer
computeDowntime _day list = case getDaySchedule _day list of
  [] -> 0
  _schedule ->
    let
      _first = head _schedule
      _last = last _schedule
    in diffTimeToPicoseconds $
      diffTimeOfDay (endTime _last) (startTime _first) - blockListLength _schedule

blockListLength :: [TimeBlock] -> DiffTime
blockListLength [] = 0
blockListLength (first:rest) =
  (diffTimeOfDay (endTime first) (startTime first)) + blockListLength rest

-- Library doesnt already have one
diffTimeOfDay :: TimeOfDay -> TimeOfDay -> DiffTime
diffTimeOfDay t1 t2 = timeOfDayToTime t1 - timeOfDayToTime t2
