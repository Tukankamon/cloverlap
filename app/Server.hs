{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Text.Lazy (pack)
import qualified Data.Vector as V
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static
import Optimize (bestSchedule)
import Parser (getCoursesFromBytes)
import Types

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson (eitherDecode)

data Request = Request
  { csv :: String
  , args :: ArgsInput
  } deriving (Show, Generic)
instance FromJSON Request
instance ToJSON Request

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
    Right (Request csvData arguments) -> do
      liftIO $ putStrLn "Parsed JSON successfully"

      -- #TODO create a getCoursesFromX so you don't need this long conversion
      case getCoursesFromBytes (BL8.pack csvData) of
       Left err -> do
         status status400
         text (pack err)
       Right courses -> do
        let result = bestSchedule (V.toList courses) (inputToArgs arguments)
        case result of
         [] -> do
          json ("No elements match criteria" :: String)
          liftIO $ putStrLn "Sent back with no results matching criteria"
         (x : xs) -> do
          json $ show (length xs) : getNamesFromSchedule x 
          liftIO $ putStrLn "Correctly sent back response"
