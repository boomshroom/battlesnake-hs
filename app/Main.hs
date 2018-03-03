{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib
import System.Environment
import Web.Scotty
import Data.Aeson (decode)
import Data.Maybe

getPort :: IO Int
getPort = fmap (maybe 9000 read) $ lookupEnv "PORT"

main :: IO ()
main = do
	port <- getPort
	scotty port $ do
		matchAny "/start" start
		matchAny "/move" move
		matchAny "/" $ text "Hello World"

start :: ActionM ()
start = fmap (fromJust . decode) body >>= (json . Lib.start)

move :: ActionM ()
move = fmap (fromJust . decode) body >>= (json . Lib.move)
