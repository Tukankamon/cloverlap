module Main (main) where

import Parser (getCoursesVector)
import Logic.Optimize (bestSchedule)
import Print (showBestSchedule)
import qualified Data.Vector as V
import Options.Applicative

data Args = Args {
  verbose :: Bool,
  classRest :: Integer,
  examRest :: Integer,
  maxClasses :: Integer,
  minClasses :: Integer
}

flags :: Parser Args
flags = Args
  <$> switch
    ( long "verbose"
    <> short 'v'
    <> help "Whether to enable verbose mode when printing out data" )
  <*> option auto
    ( long "class-rest"
    <> help "How much time in minutes minimum in between classes"
    <> showDefault
    <> value 10
    <> metavar "INTEGER" )
  <*> option auto
    ( long "exam-rest"
    <> help "How much time in days minimum in between exams"
    <> showDefault
    <> value 1
    <> metavar "INTEGER" )
  <*> option auto
    ( long "max-classes"
    <> short 'M'
    <> help "Maximum classes to sign up for, this includes those that wont be attended to but will self study to go to the exam"
    <> showDefault
    <> value 8
    <> metavar "INTEGER" )
  <*> option auto
    ( long "min-classes"
    <> short 'm'
    <> help "Absolute minimum amount of classes to attend"
    <> showDefault
    <> value 6
    <> metavar "INTEGER" )

-- #TODO improve function name
opts :: ParserInfo Args
opts = info (flags <**> helper)
  ( fullDesc
  <> progDesc "Compute the most optimal class schedule from a list of courses"
  <> header "I dont know what to put here" )

main :: IO ()
main = do
  args <- execParser opts
  result <- getCoursesVector "private.csv"
  case result of
    Left err -> putStrLn err
    Right v -> do
      let
        restValues = [classRest args, examRest args, maxClasses args, minClasses args]
      --printOverlapInList (V.toList v) defaultRest
      putStrLn $ showBestSchedule (verbose args) $ bestSchedule (V.toList v) restValues
