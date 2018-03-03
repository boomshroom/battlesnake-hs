{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.Environment
import Web.Scotty

getPort :: IO Int
getPort = fmap (maybe 9000 read) $ lookupEnv "PORT"

main :: IO ()
main = do
	port <- getPort
	scotty port $ do
		matchAny "/start" start
		matchAny "/move" move