module Main (main) where

import Types
import Parser (getCoursesVector)
import Overlap (overlapInList)
import Optimize (bestSchedule)
import qualified Data.Vector as V

import Data.Time
import Data.Maybe (fromJust)

classRest, examRest, maxClasses, minClasses :: Integer
classRest = 10 -- Minutes
examRest = 1 -- Days
maxClasses = 8
minClasses = 6

-- Should probably be a triple rather than a variable list
defaultRest :: [Integer]
defaultRest = [classRest, examRest, maxClasses, minClasses]

showDay :: TimeBlock -> String
showDay block
  | isExam block == Just False = take 3 (show $ fromJust $ weekday block)
  | isExam block == Just True =
    formatTime defaultTimeLocale "%d/%m" (fromJust $ day block)
  | otherwise = "\nERROR: NEITHER CLASS NOR EXAM IN INPUT\n"

-- Take 3 is to only have abbr like Mon for monday
showTimeBlock :: TimeBlock -> String
showTimeBlock block =
  let cut xs = take (length xs - 3) xs -- Removes last 3 from list
  in
    showDay block ++ " " ++ cut (show (startTime block))
    ++ " - " ++ cut (show (endTime block))

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

printOverlapInList :: [Course] -> [Integer] -> IO()
printOverlapInList list rests
  | overlapList == [] =
    putStrLn "No courses overlap in either classes or exams" 
  | otherwise = do
    putStrLn "These following courses overlap in the given input:"
    mapM_ formatOverlap overlapList
      where
      overlapList = overlapInList list rests
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

showBestSchedule :: Bool -> Maybe Schedule-> String
showBestSchedule _ Nothing=
  "Could not find an optimal schedule with the input conditions"
showBestSchedule verbose (Just s) = "This is the most optimal configuration:\n"
    ++ (showAllCourses s verbose) ++ "It has " ++ show (length s) ++ " total classes\n"

main :: IO ()
main = do
  result <- getCoursesVector "private.csv"
  case result of
    Left err -> putStrLn err
    Right v ->
      --printOverlapInList (V.toList v) defaultRest
      putStrLn $ showBestSchedule False $ bestSchedule (V.toList v) defaultRest
