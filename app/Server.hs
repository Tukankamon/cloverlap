{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Text.Lazy (pack)
import qualified Data.Vector as V
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static
import Optimize (bestSchedule)
import Parser (getCoursesBytes)
import Types
import Data.Time

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8

data Request = Request
  { csv :: String
  , args :: ArgsInput
  } deriving (Show, Generic)
instance FromJSON Request
instance ToJSON Request

data Response = Response
	{ title :: String
	,	classes :: [String]
	, calendar :: [[TimeBlock]]
	, exams :: [Day]
	} deriving (Show, Generic)
instance ToJSON Response

handleResponse :: Request -> ActionM ()
handleResponse (Request csvData arguments) = do
	liftIO $ putStrLn "Parsed JSON successfully"
	-- #TODO create a getCoursesFromX so you don't need this long conversion
	case getCoursesBytes (BL8.pack csvData) of
	 Left err -> do
		 status status400
		 text (pack err)
	 Right courses -> do
		let result = bestSchedule (V.toList courses) (inputToArgs arguments)
		case result of
		 [] -> do
			json $ Response
				{title = "No elements match criteria"
				, classes = []
				, calendar = []
				, exams = []
			}
			liftIO $ putStrLn "Sent back with no results matching criteria"
		 (x : xs) -> do
			json $ Response
				{title = show (length xs)
				, classes = getNamesFromSchedule x
				, calendar = []
				, exams = []
				}
			liftIO $ putStrLn "Correctly sent back response"

main :: IO ()
main = scotty 8080 $ do
 middleware $ staticPolicy (addBase "frontend/static")
 get "/" $ file "frontend/index.html"

 post "/api/optimize" $ do
  rq <- body
  liftIO $ putStrLn "Request Recieved"
  --liftIO $ putStrLn $ show rq
  case eitherDecode rq of
    Left err -> do
      status status400
      text $ pack err
    Right r -> handleResponse r

