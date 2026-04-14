{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.Static

main :: IO ()
main = scotty 8080 $ do

  middleware $ staticPolicy (addBase "frontend/static")
  get "/" $ file "frontend/index.html"
