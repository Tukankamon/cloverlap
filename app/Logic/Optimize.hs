module Logic.Optimize (bestSchedule) where

import Types
import Logic.Overlap
import Data.Time
import Data.List

-- #TODO doing this with fold' can be a lot quicker
-- #TODO dont be so strict, if the conditions dont return any then loosen them a bit
bestSchedule :: Schedule -> Args -> [Schedule]
bestSchedule [] _ = []
bestSchedule set args = case generateAllCombinations set args of
  [] -> []
  list -> [maximumBy downtimeOrdering list]
  where
    -- Add more conditions as needed
    -- #TODO make it so it is not so ordered in importance by priority, downtime, length rather take all of them into account at once
    downtimeOrdering :: Schedule -> Schedule -> Ordering
    downtimeOrdering s1 s2
      | computePriority s1 < computePriority s2 = LT
      | computePriority s1 > computePriority s2 = GT
      | weekDowntimePerClass s1 < weekDowntimePerClass s2 = LT
      | weekDowntimePerClass s1 > weekDowntimePerClass s2 = GT
      | otherwise = EQ

generateAllCombinations :: Schedule -> Args -> [Schedule]
generateAllCombinations list args =
    [ pick | pick <- subsequences list,
    length pick <= fromInteger (maxClasses args),
    computeAttendance pick >= minClasses args,
    -- There is probably a better way to do the following
    [ course | course<-pick, semester course == trimester args]  == pick,
    overlapInList pick args == []]

computePriority :: Schedule -> Integer
computePriority schedule = sum [priority course | course<-schedule ]

-- Doesnt apply for exams
computeAttendance :: Schedule -> Integer
computeAttendance schedule =
  toInteger $ length [course | course<-schedule, skip_class course == False ]

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
