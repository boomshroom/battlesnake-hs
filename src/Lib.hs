module Lib (start, move) where

import Api
import Data.Maybe

start :: StartReq -> StartResp
start _ = StartResp { color = "#FF0000", head_type = "pixel", tail_type = "pixel" }

move :: MoveReq -> MoveResp
move req = MoveResp $
	let
		List (head : _) = body $ you req
		List f = food req
		p = fromMaybe (Point 0 0) $ shortest head f
		dir = goto head p
	in
		if is_safe req $ offset head dir then dir
		else if is_safe req $ offset head DUp then DUp
		else if is_safe req $ offset head DDown then DDown
		else if is_safe req $ offset head DLeft then DLeft
		else if is_safe req $ offset head DRight then DRight
		else DUp -- Dead

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

is_safe :: MoveReq -> Point -> Bool
is_safe board p@(Point x y) = if x < 0 || x > width board || y < 0 || y > width board then False
	else let List s = snakes board
	in all (/= p)$ s >>= (\s -> case body s of List p -> p)

offset :: Point -> Direction -> Point
offset (Point x y) DUp = Point x (y-1)
offset (Point x y) DRight = Point (x+1) y
offset (Point x y) DLeft = Point (x-1) y
offset (Point x y) DDown = Point x (y+1)