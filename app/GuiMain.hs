{-# LANGUAGE OverloadedStrings #-} -- To use Strings as Text
import Graphics.UI.TinyFileDialogs
import System.Exit (exitSuccess)
import Data.Text
import Control.Monad (void)
import Parser (getCoursesVector)
import Optimize (bestSchedule)
import Print (showSchedule, showWeekSchedules)
import Types
import qualified Data.Vector as V

handleInput :: Text -> IO ()
handleInput path = do
  result<-getCoursesVector (unpack path)
  case result of
    Left err -> putStrLn err
    Right v -> do
      let args = (Args (unpack path) False 10 1 8 6 1 False)
      let list = V.toList v
      let best_schedule = bestSchedule list args
      case best_schedule of
        [] -> putStrLn "Can't find schedule for given input"
        s -> do
          putStrLn $ showSchedule list args
          putStrLn $ showWeekSchedules s 1 args
  


-- Ugly imperative programming (ew)
main :: IO()
main = do
  --start<-
    --messageBox "Choose file" "Choose a csv containing the possible courses" Info OKCancel
  mPath<-
    -- The empty list is suposed to contain "*.csv" but that hangs the program
    -- by starting this process that hangs indefinately:
    -- user ... zenity --file-selection --title=Select csv file to process --file-filter=text files | *.csv  --file-filter=All files | *
    openFileDialog "Select csv file to process" "" [] "text files" False
  case mPath of
    Nothing -> do
      _<-messageBox "Exit" "No file was selected" Info OK
      exitSuccess
    Just (path:_) -> do
      handleInput path
      void $ messageBox "Done" "Finished processing" Info OK
      exitSuccess
    _ -> do
      void $ messageBox "Error" "Unexpected error" Error OK
      exitSuccess
