module Main (main) where

import Types
import Parser (getCoursesVector)
import Logic (overlapInVector)

import qualified Data.Vector as V
import Data.Time
import Data.Maybe (fromJust)

classRest, examRest :: Integer
classRest = 10 -- Minutes
examRest = 1 -- Days

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
printCourse :: Course -> Bool ->  IO ()
printCourse course False = putStrLn $ 
  name course
printCourse course True = putStrLn $
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

printAllCourses :: (V.Vector Course) -> Bool -> IO ()
printAllCourses vector verbosity =
  V.forM_ vector $ \course -> printCourse course verbosity

printOverlapInVector :: V.Vector Course -> [Integer] -> IO()
printOverlapInVector v rests
  | overlapList == [] =
    putStrLn "No courses overlap in either classes or exams" 
  | otherwise = do
    putStrLn "These following courses overlap in the given input:"
    mapM_ formatOverlap overlapList
      where
      overlapList = overlapInVector v rests
      formatOverlap :: (Course, Course, [(TimeBlock, TimeBlock)]) -> IO()
      formatOverlap (course1, course2, list) = do
        putStrLn ""
        printCourse course1 False
        printCourse course2 False
        putStrLn $ "With the following contradictions:\n" ++ showTimeBlockContr list

showTimeBlockContr :: [(TimeBlock, TimeBlock)] -> String
showTimeBlockContr [] = "\n"
showTimeBlockContr (firstPair:list) =
  showTimeBlock (fst firstPair) ++ " contradicts with: "
  ++ showTimeBlock (snd firstPair) ++ "\n" ++ showTimeBlockContr list

main :: IO ()
main = do
  result <- getCoursesVector "example.csv"
  case result of
    Left err -> putStrLn err
    Right v -> printOverlapInVector v [classRest, examRest]
