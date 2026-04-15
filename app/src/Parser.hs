-- Important
{-# LANGUAGE OverloadedStrings #-}
-- If not would have to move a lot over to Types
{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (getCoursesVector, getCoursesBytes) where

import qualified Data.ByteString.Char8 as BS -- IDK why I need this
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Csv
import Data.Maybe (isJust)
import Data.Time
import qualified Data.Vector as V
import Types

-- Tells how to turn csv into the Class type
instance FromNamedRecord Course where
 parseNamedRecord r =
  Course
   <$> r .: "name"
   <*> r .: "semester"
   <*> r .: "time1"
   <*> r .: "time2"
   <*> r .: "time3"
   <*> r .: "exam1"
   <*> r .: "exam2"
   <*> r .: "exam3"
   <*> r .: "skip_class"
   <*> r .: "priority"

isComment :: BL8.ByteString -> Bool
isComment line = case BL8.uncons line of
  Just ('#', _) -> True
  _             -> False

removeComments :: BL.ByteString -> BL.ByteString
removeComments =
  BL8.unlines
  . filter (not . isComment)
  . BL8.lines

instance FromField TimeBlock where
 parseField bs =
  let ws = words $ BS.unpack bs
   in case parseTimeBlock ws of
       Just block -> pure block
       Nothing -> fail ("Cannot parse time block: " ++ show ws ++ "\n")

instance FromField Bool where
 parseField b
  | b == "true" = pure True
  | b == "false" = pure False
  | otherwise = fail ("Cannot parse bool" ++ show b)

getCoursesVector :: FilePath -> IO (Either String (V.Vector Course))
getCoursesVector file = do
 csvData <- BL.readFile file
 let cleaned = removeComments csvData
 case decodeByName cleaned of
  Left err -> return $ Left err
  Right (_, v) -> return $ Right v

-- To read files sent through HTTP
getCoursesBytes :: BL.ByteString -> Either String (V.Vector Course)
getCoursesBytes csvData = case decodeByName (removeComments csvData) of
 Left err -> Left err
 Right (_, v) -> Right v

parseTimeBlock :: [String] -> Maybe TimeBlock
parseTimeBlock [] = Nothing
parseTimeBlock (x:xs)
 | startsWithWeekDay x = parseClass (x:xs)
 | otherwise = parseExam (x:xs)

parseClass :: [String] -> Maybe TimeBlock
parseClass (_weekday : start : end : _) = do
 wd <- parseDayAbbr _weekday
 st <- parseTimeOfDay start
 et <- parseTimeOfDay end
 return $ TimeBlock (Just wd) Nothing st et
parseClass _ = Nothing

-- #TODO join this and the above into one
parseExam :: [String] -> Maybe TimeBlock
-- This allows exams to be declared just with the date, no start and end times
parseExam [_day] = Just $ TimeBlock Nothing (parseDay _day) midday midday
parseExam (_day : start : end : _) = do
 d <- parseDay _day
 st <- parseTimeOfDay start
 et <- parseTimeOfDay end
 return $ TimeBlock Nothing (Just d) st et
parseExam _ = Nothing

parseDay :: String -> Maybe Day
parseDay s = parseTimeM True defaultTimeLocale "%d/%m" s

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay s = parseTimeM True defaultTimeLocale "%H:%M" s

startsWithWeekDay :: String -> Bool
startsWithWeekDay s = isJust $ parseDayAbbr s

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
