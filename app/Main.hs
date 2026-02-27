module Main (main) where

import Types
import Parser ()

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Time
import Data.Maybe (fromJust)

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

printCourse :: Course -> IO ()
printCourse course = putStrLn $
  name course ++ " has classes on " ++ showTimeBlock (time1 course) ++ ", "
  ++ showTimeBlock (time2 course)
  ++ " and " ++ showTimeBlock (time3 course)
  ++ " with exams on " ++ showTimeBlock (exam1 course) ++ " and "
  ++ showTimeBlock (exam2 course) ++ ". Will class be attended?: " ++ skip_class course

printAllCourses :: (V.Vector Course) -> IO ()
printAllCourses v = V.forM_ v $ \course -> printCourse course

getCourseVector :: FilePath -> IO (Either String (V.Vector Course))
getCourseVector file = do
  csvData <- BL.readFile file
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_ ,v) -> return $ Right v

main :: IO ()
main = do
  result <- getCourseVector "example.csv"
  putStrLn "HI"
  case result of
    Left err -> putStrLn err
    Right v -> printAllCourses v
