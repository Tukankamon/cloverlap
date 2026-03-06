module Print
  (showSchedule,
  showAllCourses,
  showOverlapInList,
  showWeekSchedules)
where

import Types
import Logic.Overlap (overlapInList)
import Logic.Optimize (bestSchedule)
import Data.Time
import Data.Maybe (fromJust)

showDay :: TimeBlock -> String
showDay block
  | isExam block == Just False = take 3 (show $ fromJust $ weekday block)
  | isExam block == Just True =
    formatTime defaultTimeLocale "%d/%m" (fromJust $ day block) | otherwise = "\nERROR: NEITHER CLASS NOR EXAM IN INPUT\n"

-- Take 3 is to only have abbr like Mon for monday
showTimeBlock :: TimeBlock -> String
showTimeBlock block =
  let cut xs = take (length xs - 3) xs -- Removes last 3 from list
  in
    showDay block ++ " " ++ cut (show (startTime block))
    ++ " - " ++ cut (show (endTime block))

showTimeBlockList :: [TimeBlock] -> String
showTimeBlockList [] = ""
showTimeBlockList (first:rest) = showTimeBlock first ++ "\n" ++ showTimeBlockList rest

-- Bool is verbosity
showCourse :: Course -> Bool -> String
showCourse course False = name course
showCourse course True =
  name course ++ " has classes on " ++ showTimeBlock (time1 course) ++ ", "
  ++ showTimeBlock (time2 course)
  ++ maybeTimeBlock (time3 course)
  ++ " with exams on " ++ showTimeBlock (exam1 course) ++ ", "
  ++ showTimeBlock (exam2 course)
  ++ maybeTimeBlock (exam3 course) ++ ". Will class be attended?: "
  ++ show (skip_class course)
  ++ ". Priority: " ++ (show (priority course))
  where
    maybeTimeBlock :: Maybe TimeBlock -> String
    maybeTimeBlock _timeBlock = case _timeBlock of
      Just tb -> " and " ++ showTimeBlock tb
      Nothing -> ""

showAllCourses :: Schedule -> Bool -> String
showAllCourses [] _ = ""
showAllCourses (first:rest) verbosity =
  (showCourse first verbosity) ++ "\n" ++ showAllCourses rest verbosity

showOverlapInList :: Schedule -> Args -> String
showOverlapInList list args
  | overlapList == [] =
    "No courses overlap in either classes or exams" 
  | otherwise =
    "These following courses overlap in the given input:\n" ++
    concatMap formatOverlap overlapList
      where
      overlapList = overlapInList list args
      formatOverlap :: (Course, Course, [(TimeBlock, TimeBlock)]) -> String
      formatOverlap (course1, course2, contradictions) =
        showCourse course1 False ++ ", " ++ showCourse course2 False
        ++ " with the following contradictions:\n"
        ++ showTimeBlockContr contradictions

showTimeBlockContr :: [(TimeBlock, TimeBlock)] -> String
showTimeBlockContr [] = ""
showTimeBlockContr (firstPair:list) =
  showTimeBlock (fst firstPair) ++ " contradicts with: "
  ++ showTimeBlock (snd firstPair) ++ "\n" ++ showTimeBlockContr list

-- IDk if this should do the computing or if the output of bestSchedule should be passed to this in main
showSchedule :: Schedule -> Args -> String
showSchedule set args =
  let errString = "Could not find an optimal schedule with the input conditions\n" in
  case (bestSchedule set args, (verbose args)) of
  ([], False) -> errString
  -- "The best" in terms of downtime not overlapping #TODO
  ([], True) -> errString ++ "This is the best that can be found:\n"
    ++ showOverlapInList set args
  (list, _) -> (showAllCourses s (verbose args)) ++ "It has "
    ++ show (length s) ++ " total classes with "
    ++ show (length (filter (not . skip_class) s)) ++ " to be attended\n"
    where s = head list -- #TODO print all not just head

showDaySchedule :: DayOfWeek -> Schedule -> Args -> String
showDaySchedule _day list args
  | list == [] || daily_courses == [] = "No classes on " ++ show _day ++ "\n"
  | otherwise = "The schedule for " ++ show _day ++ " is:\n"
  ++ showAllCourses daily_courses (verbose args) ++ "\n"
  where daily_courses = getCoursesFromDay _day list

showWeekSchedule :: Schedule -> Args -> String
showWeekSchedule schedule args =
  unwords [ showDaySchedule _day schedule args | _day<-[Monday ..Sunday] ]

-- Can this be done without a counter?
showWeekSchedules :: [Schedule] -> Integer -> Args -> String
showWeekSchedules [] _ _ = ""
showWeekSchedules (first:rest) counter args =
  "--- Schedule option " ++ show counter ++ " ---\n" ++ showWeekSchedule first args
  ++ showWeekSchedules rest (counter+1) args

