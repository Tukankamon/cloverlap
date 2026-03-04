module Print
  (showSchedule,
  showAllCourses,
  printOverlapInList,
  showWeekSchedule)
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

printOverlapInList :: [Course] -> Args -> IO()
printOverlapInList list args
  | overlapList == [] =
    putStrLn "No courses overlap in either classes or exams" 
  | otherwise = do
    putStrLn "These following courses overlap in the given input:"
    mapM_ formatOverlap overlapList
      where
      overlapList = overlapInList list args
      formatOverlap :: (Course, Course, [(TimeBlock, TimeBlock)]) -> IO()
      formatOverlap (course1, course2, contradictions) = do
        putStrLn ""
        putStrLn $ showCourse course1 False
        putStrLn $ showCourse course2 False
        putStrLn $
          "With the following contradictions:\n" ++ showTimeBlockContr contradictions

showTimeBlockContr :: [(TimeBlock, TimeBlock)] -> String
showTimeBlockContr [] = ""
showTimeBlockContr (firstPair:list) =
  showTimeBlock (fst firstPair) ++ " contradicts with: "
  ++ showTimeBlock (snd firstPair) ++ "\n" ++ showTimeBlockContr list

-- IDk if this should do the computing or if the output of bestSchedule should be passed to this in main
showSchedule :: Schedule -> Args -> String
showSchedule set args =
  case bestSchedule set args of
  Nothing ->
    "Could not find an optimal schedule with the input conditions:\n" ++ show args
  (Just s) -> "This is the most optimal configuration:\n"
    ++ (showAllCourses s (verbose args)) ++ "It has " ++ show (length s) ++ " total classes\n"

showDaySchedule :: DayOfWeek -> Schedule -> String
showDaySchedule _day list = case getDaySchedule _day list of
  [] -> "No classes on " ++ show _day ++ "\n"
  blocklist ->
    "The schedule for " ++ show _day ++ " is:\n" ++ showTimeBlockList blocklist ++ "\n"

showWeekSchedule :: Schedule -> String
showWeekSchedule schedule =
  unwords [ showDaySchedule _day schedule | _day<-[Monday ..Sunday] ]

