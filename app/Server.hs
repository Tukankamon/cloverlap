{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

-- import Web.Scotty.Trans (param, rescue)

import Data.Text.Lazy (pack)
import qualified Data.Vector as V
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static
import Optimize (bestSchedule)
import Parser (getCoursesFromBytes)
import Types

main :: IO ()
main = scotty 8080 $ do
 middleware $ staticPolicy (addBase "frontend/static")

 get "/" $ file "frontend/index.html"

 post "/api/optimize" $ do
  csvData <- body
  liftIO $ putStrLn "📄 CSV file received"

  -- Read args from query params with defaults (the functions after rescue are the defaults)
  {-
      trimester_  <- param "trimester"  `rescue` (\_ -> return 1)
      maxClasses_ <- param "maxClasses" `rescue` (\_ -> return 6)
      minClasses_ <- param "minClasses" `rescue` (\_ -> return 3)
      classRest_  <- param "classRest"  `rescue` (\_ -> return 1)
      examRest_   <- param "examRest"   `rescue` (\_ -> return 1)
  -}
  let args = Args {
      input = ""
      , Types.verbose = False
      , classRest = 10
      , examRest = 1
      , maxClasses = 8
      , minClasses = 5
      , trimester = 1
      , loosen = False
      }

  case getCoursesFromBytes csvData of
   Left err -> do
     status status400
     text (pack err)
   Right courses -> do
    let result = bestSchedule (V.toList courses) args
    case result of
     [] -> do
      json ("No elements match criteria" :: String)
      liftIO $ putStrLn "Sent back with no results matching criteria"
     (x : _) -> do
      json $ getNamesFromSchedule x
      liftIO $ putStrLn "Correctly sent back response"
