{-# LANGUAGE OverloadedStrings #-} -- Important
{-# OPTIONS_GHC -Wno-orphans #-} -- If not would have to move a lot over to Types
module Parser (getCoursesVector) where

import Types
import qualified Data.ByteString.Char8 as BS -- IDK why I need this
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Data.Time
import Data.Maybe (isJust)

-- Tells how to turn csv into the Class type
instance FromNamedRecord Course where
  parseNamedRecord r = Course
    <$> r .: "name"
    <*> r .: "time1"
    <*> r .: "time2"
    <*> r .: "time3"
    <*> r .: "exam1"
    <*> r .: "exam2"
    <*> r .: "skip_class"
    <*> r .: "priority"

instance FromField TimeBlock where
  parseField bs = 
    let ws = words $ BS.unpack bs
    in case parseTimeBlock ws of
      Just block -> pure block
      Nothing -> fail "Cannot parse time block"

instance FromField Bool where
  parseField b
    | b == "true" = pure True
    | b == "false" = pure False
    | otherwise = fail "Cannot parse bool"

getCoursesVector :: FilePath -> IO (Either String (V.Vector Course))
getCoursesVector file = do
  csvData <- BL.readFile file
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_ ,v) -> return $ Right v

parseTimeBlock :: [String] -> Maybe TimeBlock
parseTimeBlock [] = Nothing
parseTimeBlock xs
  | startsWithWeekDay (xs !! 0) = parseClass xs
  | otherwise = parseExam xs

parseClass :: [String] -> Maybe TimeBlock
parseClass (_weekday:start:end:_) = do
  wd <- parseDayAbbr _weekday
  st <- parseTimeOfDay start
  et <- parseTimeOfDay end
  return $ TimeBlock (Just wd) Nothing st et
parseClass _ = Nothing

-- #TODO join this and the above into one
parseExam :: [String] -> Maybe TimeBlock
parseExam (_day:start:end:_) = do
  d <- parseDay _day
  st <- parseTimeOfDay start
  et <- parseTimeOfDay end
  return $ TimeBlock Nothing (Just d) st et
parseExam _ = Nothing

parseDay :: String -> Maybe Day
parseDay s = parseTimeM True defaultTimeLocale "%d/%m" s

parseDayAbbr :: String -> Maybe DayOfWeek
parseDayAbbr s
  | s == "Mon" = Just Monday
  | s == "Tue" = Just Tuesday
  | s == "Wed" = Just Wednesday
  | s == "Thu" = Just Thursday
  | s == "Fri" = Just Friday
  | s == "Sat" = Just Saturday
  | s == "Sun" = Just Sunday
  | otherwise = Nothing

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay s = parseTimeM True defaultTimeLocale "%H:%M" s

startsWithWeekDay :: String -> Bool
startsWithWeekDay s = isJust $ parseDayAbbr s
