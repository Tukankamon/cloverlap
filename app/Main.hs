module Main (main) where

import Parser (getCoursesVector)
import Print (showSchedule, showWeekSchedules, showOverlapInList)
import qualified Data.Vector as V
import Options.Applicative
import Logic.Optimize (bestSchedule)
import Types

flags :: Parser Args
flags = Args
  <$> strOption
    ( long "input"
    <> short 'i'
    <> help "File name (not path) of the csv file to parse)"
    <> showDefault
    <> value "example.csv"
    <> metavar "FILENAME" )
  <*> switch
    ( long "verbose"
    <> short 'v'
    <> help "Whether to enable verbose mode when printing out data" )
  <*> option auto
    ( long "class-rest"
    <> help "How much time in minutes minimum in between classes"
    <> showDefault
    <> value 10
    <> metavar "INT" )
  <*> option auto
    ( long "exam-rest"
    <> help "How much time in days minimum in between exams. A value of 0 means allowing exams on the same day"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> option auto
    ( long "max-classes"
    <> short 'M'
    <> help "Maximum classes to sign up for, this includes those that wont be attended to but will self study to go to the exam"
    <> showDefault
    <> value 8
    <> metavar "INT" )
  <*> option auto
    ( long "min-classes"
    <> short 'm'
    <> help "Absolute minimum amount of classes to attend"
    <> showDefault
    <> value 5
    <> metavar "INT" )
  <*> option auto
    ( long "semester"
    <> short 's'
    <> help "Semester to analyze (1 or 2)"
    <> value 1
    <> metavar "INT" )
  <*> switch
    ( long "loosen"
    <> short 'l'
    <> help "If the program fails with the given restrictions, it will lower them until it finds a match")

-- #TODO improve function name
opts :: ParserInfo Args
opts = info (flags <**> helper)
  ( fullDesc
  <> progDesc "Compute the most optimal class schedule from a list of courses"
  <> header "University course picker / optimizer" )

looser :: Args -> Maybe Args
looser (Args _ _ _ _ _ _ _ False) = Nothing
looser (Args f v cRest eRest max min s True)
  | cRest == 0 && eRest == 0 = Nothing
  | otherwise = Just $ Args f v newCRest newERest newMax newMin s True
  where
  newCRest = cRest `div` 2
  newERest = eRest `div` 2
  newMin = min -1
  newMax = max +1

-- #TODO better function name
mainIsh :: Schedule -> Args -> IO()
mainIsh list args = do
  let best_schedule = bestSchedule list args
  case best_schedule of
    [] ->
      if loosen args == False then do
        putStrLn "Can't find schedule for given input"
      else
      case looser args of
          Nothing -> putStrLn "Args can't be any looser"
          Just a -> do
            putStrLn $ "Could not show the week schedule, trying with looser args"
            mainIsh list a
    s -> do
      putStrLn $ showSchedule list args
      putStrLn ""
      putStrLn $ showWeekSchedules s 1 args
      -- idk how to not have the else
      if verbose args then putStrLn $ show $ args else putStr ""

main :: IO ()
main = do
  args <- execParser opts
  result <- getCoursesVector (input args)
  case result of
    Left err -> putStrLn err
    Right v -> mainIsh (V.toList v) args
      --printOverlapInList (V.toList v) defaultRest
