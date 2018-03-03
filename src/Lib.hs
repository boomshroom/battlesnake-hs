{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( start
    , move
    ) where

import Web.Scotty
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Maybe

data StartReq = StartReq { game_id :: Int, width :: Int, height :: Int } deriving (Generic, Show)
data StartResp = StartResp { color :: String, head_type :: String, tail_type :: String } deriving (Generic, Show)

instance FromJSON StartReq
instance ToJSON StartResp

start :: ActionM ()
start = do
	req <- fmap (fromJust . decode) body :: ActionM StartReq
	json StartResp { color = "#FF0000", head_type = "pixel", tail_type = "pixel" }

move :: ActionM ()
move = return ()
