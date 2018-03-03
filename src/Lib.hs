module Lib (start, move) where

import Api
import Data.Maybe

start :: StartReq -> StartResp
start _ = StartResp { color = "#FF0000", head_type = "pixel",
	tail_type = "pixel", head_url = "http://i0.kym-cdn.com/entries/icons/original/000/024/812/dontforgetme2.png",
	taunt = "Think you can take me?" }

move :: MoveReq -> MoveResp
move req = MoveResp { action = dir req}
	
dir :: MoveReq -> Direction
dir req = let
		List (head : _) = body $ you req
		List f = food req
		p = fromMaybe (Point 0 0) $ shortest head f
		(dir1, dir2) = goto head p
	in
		if is_safe req $ offset head dir1 then dir1
		else if is_safe req $ offset head dir2 then dir2
		else fallback req


fallback :: MoveReq -> Direction
fallback board = let List (head : _) = body $ you board in
	if is_safe board $ offset head DUp then DUp
	else if is_safe board $ offset head DDown then DDown
	else if is_safe board $ offset head DLeft then DLeft
	else if is_safe board $ offset head DRight then DRight
	else DUp -- Dead

dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

shortest :: Point -> [Point] -> Maybe Point
shortest here = foldl (\s p -> Just $ case s of
	Just s -> if dist here p < dist here s then p else s
	Nothing -> p) Nothing

goto :: Point -> Point -> (Direction, Direction)
goto (Point x1 y1) (Point x2 y2) =
	let
		dy = y2 - y1
		dx = x2 - x1
		v = if dy > 0 then DDown else DUp
		h = if dx > 0 then DRight else DLeft
	in if abs dx < abs dy then (v, h) else (h, v)
			

is_safe :: MoveReq -> Point -> Bool
is_safe board p@(Point x y) = if x < 0 || x >= width board || y < 0 || y >= width board then False
	else let
		List s = snakes board
		me = you board
	in 
		if any (\s -> case body s of List (h : _) -> adjacent h p && snake_length s >= snake_length me) $
			filter (\s -> snake_id s /= snake_id me) s 
		then False
		else all (/= p)$ s >>= (\s -> case body s of List p -> p)

offset :: Point -> Direction -> Point
offset (Point x y) DUp = Point x (y-1)
offset (Point x y) DRight = Point (x+1) y
offset (Point x y) DLeft = Point (x-1) y
offset (Point x y) DDown = Point x (y+1)

adjacent :: Point -> Point -> Bool
adjacent p1 p2 = any (\d -> offset p2 d == p1) [DUp, DDown, DRight, DLeft] 