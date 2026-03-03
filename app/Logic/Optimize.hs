module Logic.Optimize (bestSchedule) where

import Types
import Logic.Overlap
import Data.Time
import Data.List

-- #TODO check for multiple possible results
-- #TODO dont be so strict, if the conditions dont return any then loosen them a bit
bestSchedule :: Schedule -> [Integer] -> Maybe Schedule
bestSchedule set rests
  | list == [] = Nothing
  | length rests /= 4 = Nothing --should be: classRest, examRest, maxClasses, minClasses
  | otherwise = Just $ head $ sortBy downtimeSort list
  where
    list = generateAllCombinations set rests
    -- Add more conditions as needed
    downtimeSort :: Schedule -> Schedule -> Ordering
    downtimeSort s1 s2
      | computePriority s1 < computePriority s2 = GT
      | computePriority s1 > computePriority s2 = LT
      | weekDowntimePerClass s1 <= weekDowntimePerClass s2 = GT
      | otherwise = LT

generateAllCombinations :: Schedule -> [Integer] -> [Schedule]
generateAllCombinations list rests =
  let
    classRest = rests !! 0
    examRest = rests !! 1
    maxClasses = rests !! 2
    minClasses = rests !! 3
  in
    [ pick | pick <- subsequences list,
    length pick <= fromInteger maxClasses,
    computeAttendance pick >= minClasses,
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
      occupiedTime = blockListLength _schedule
    in diffTimeToPicoseconds $ timeOfDayToTime (endTime _last)
    - timeOfDayToTime (startTime _first) - occupiedTime


getDaySchedule :: DayOfWeek -> Schedule -> [TimeBlock]
getDaySchedule _day list =
  sort $ [block | course<-list,
  block<-getBlockFromCourse course, Just _day == weekday block ]

blockListLength :: [TimeBlock] -> DiffTime
blockListLength [] = 0
blockListLength (first:rest) = (blockLength first) + blockListLength rest

-- Maybe could have this directly in blockListLength
blockLength :: TimeBlock -> DiffTime
blockLength block =
  timeOfDayToTime (endTime block) - timeOfDayToTime(startTime block)
