module Lib (start, move) where

import Api
import Data.Maybe

start :: StartReq -> StartResp
start _ = StartResp { color = "#FF0000", head_type = "pixel", tail_type = "pixel" }

move :: MoveReq -> MoveResp
move req =
	let
		List (head : _) = body $ you req
		List f = food req
		p = fromMaybe (Point 0 0) $ shortest head f
	in
		MoveResp {action = goto head p}

dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

shortest :: Point -> [Point] -> Maybe Point
shortest here = foldl (\s p -> Just $ case s of
	Just s -> if dist here p < dist here s then p else s
	Nothing -> p) Nothing

goto :: Point -> Point -> Direction
goto (Point x1 y1) (Point x2 y2) =
	let
		dy = y2 - y1
		dx = x2 - x1
	in
		if abs dx < abs dy then
			if dy > 0 then DDown else DUp
		else
			if dx > 0 then DRight else DLeft