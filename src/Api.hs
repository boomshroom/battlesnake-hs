{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
	( StartReq(..)
	, StartResp(..)
	, MoveReq(..)
	, MoveResp(..)
	, Point(..)
	, List(..)
	, Snake(..)
	, Direction(..)
	) where

import Data.Aeson
import GHC.Generics
import Data.HashMap.Strict

data StartReq = StartReq { game_id :: Int, start_width :: Int, start_height :: Int } deriving (Generic, Show)
data StartResp = StartResp { color :: String, head_type :: String, tail_type :: String, head_url :: String, taunt :: String } deriving (Generic, Show)

data MoveReq = MoveReq { food :: List Point, height :: Int, snakes :: List Snake, turn :: Int, width :: Int, you :: Snake} deriving (Generic, Show)
data MoveResp = MoveResp { action :: Direction } deriving (Generic, Show)

data Point = Point Int Int deriving (Generic, Show, Eq)

data List a = List [a] deriving (Generic, Show)

data Snake = Snake { body :: List Point, health :: Int, snake_id :: String, snake_length :: Int, name :: String, s_taunt :: Maybe String} deriving (Generic, Show)

data Direction = DUp | DLeft | DDown | DRight deriving (Show)

instance ToJSON Direction where
	toJSON DUp = "up"
	toJSON DLeft = "left"
	toJSON DDown = "down"
	toJSON DRight = "right"

instance FromJSON StartReq where
	parseJSON = withObject "StartReq" $ \v -> do
		width <- v .: "width"
		height <- v .: "height"
		id <- v .: "game_id"
		return StartReq { game_id = id, start_width = width, start_height = height}

instance ToJSON StartResp

instance FromJSON MoveReq
instance ToJSON MoveResp where
	toJSON o = object [ "move" .= action o ]

instance FromJSON a => FromJSON (List a) where
	parseJSON = withObject "List" (\v -> fmap List $ v .: "data")

instance FromJSON Snake where
	parseJSON = withObject "Snake" $ \v -> do
		body <- v .: "body"
		health <- v .: "health"
		id <- v .: "id"
		length <- v .: "length"
		name <- v .: "name"
		taunt <- v .: "taunt"
		return Snake {body = body, health = health, snake_id = id, snake_length = length, name = name, s_taunt = taunt}

instance FromJSON Point where
	parseJSON = withObject "Point" $ \v -> do
		x <- v .: "x"
		y <- v .: "y"
		return $ Point x y