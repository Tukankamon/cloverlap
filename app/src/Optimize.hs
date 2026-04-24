module Optimize (bestSchedule) where

import Data.List
import Data.Time
import Overlap
import Types

-- #TODO doing this with fold' can be a lot quicker
-- #TODO dont be so strict, if the conditions dont return any then loosen them a bit
bestSchedule :: [Course] -> Args -> [Schedule]
bestSchedule [] _ = []
bestSchedule set args = case generateAllCombinations set args of
 [] -> []
 list -> sortBy downtimeOrdering list
 where
 -- Add more conditions as needed
 -- #TODO make it so it is not so ordered in importance by priority, downtime, length rather take all of them into account at once
downtimeOrdering s1 s2 =
  compareDescending computePriority
  <> compareDescending weekDowntimePerClass
  <> compareDescending dayHoursSV
  where
  compareDescending f = compare (f s2) (f s1)

dayHours :: Schedule -> DayOfWeek -> Double
dayHours s d = fromIntegral $ diffTimeToPicoseconds $ blockListLength (getDaySchedule d s)

-- Standard variation (not quite since it uses abs instead of squaring)
dayHoursSV :: Schedule -> Double
dayHoursSV s = sum (map (\_day -> abs (dayHours s _day - mean)) activeDays) / len
  where
  weekHours = map (dayHours s) week
  -- Inefficient, dayHours called twice
  activeDays = filter (\d -> dayHours s d >0) week
  len = fromIntegral $ length activeDays
  mean = sum weekHours / len

kSubsequence :: Int -> [a] -> [[a]]
kSubsequence 0 _ = [[]]
kSubsequence _ [] = [[]]
kSubsequence k (x:xs) = kSubsequence k xs ++ map (x:) (kSubsequence (k-1) xs)

generateAllCombinations :: Schedule -> Args -> [Schedule]
generateAllCombinations list args =
 [ pick
 | pick <- kSubsequence max filtered
 , computeAttendance pick >= min
 , null $ overlapInList pick args
 ]
  where
  filtered = filter (\c -> semester c == trimester args) list
  max = fromInteger (maxClasses args)
  min = minClasses args 

computePriority :: Schedule -> Integer
computePriority schedule = sum $ map priority schedule

-- Doesnt apply for exams
computeAttendance :: Schedule -> Integer
computeAttendance schedule =
 toInteger $ length [course | course <- schedule, not $ skip_class course]

weekDowntimePerClass :: Schedule -> Maybe Double
weekDowntimePerClass [] = Nothing
weekDowntimePerClass list =
 let
  totalDowntime = sum [computeDowntime _day list | _day <- week]
  numberOfClasses = length list
  in
  Just (fromIntegral totalDowntime / fromIntegral numberOfClasses)

-- Couldnt find a function to diff time of TimeOfDay
computeDowntime :: DayOfWeek -> Schedule -> Integer
computeDowntime _day list = case getDaySchedule _day list of
 [] -> 0
 (x:xs) -> diffTimeToPicoseconds $ diffTimeOfDay (endTime _last) (startTime _first) - blockListLength (x : xs)
   where 
   _first = x
   _last = last $ x : xs

