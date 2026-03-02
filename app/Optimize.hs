module Optimize where

import Types
import Data.Time
import Data.Sort (sort)

-- I cant find any other function to return something other than Pico seconds
computeDowntime :: DayOfWeek -> [TimeBlock] -> Integer
computeDowntime _ [] = 0
computeDowntime _day list =
  diffTimeToPicoseconds $ (timeOfDayToTime end)
  - (timeOfDayToTime start) - getBlockLengths (list)
  where
    end = endTime $ last list
    start = startTime $ head list

getDaySchedule :: DayOfWeek -> [Course] -> [TimeBlock]
getDaySchedule _day list =
  sort $ [block | course<-list,
  block<-getBlockFromCourse course, Just _day == weekday block ]

-- Total time occupied by a list of TimeBlocks
getBlockLengths :: [TimeBlock] -> DiffTime
getBlockLengths [] = 0
getBlockLengths (first:rest) =
  timeOfDayToTime (startTime first) - timeOfDayToTime (endTime first)
  + getBlockLengths rest
