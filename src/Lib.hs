module Lib (start, move) where

import Api

start :: StartReq -> StartResp
start _ = StartResp { color = "#FF0000", head_type = "pixel", tail_type = "pixel" }

move :: MoveReq -> MoveResp
move req =
	let
		List [head : _] = body $ you req
		List food = food req
		p = shortest head food
	in
		MoveResp {action = goto head p}

dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

shortest :: Point -> [Point] -> Point
shortest here = foldl $ \s p -> if dist here p < dist here s then p else s

goto :: Point -> Point -> Direction
goto (Point x1 y1) (Point x2 y2) =
	let
		dy = y2 - y1
		dx = x2 - x1
	in
		if abs dx < abs dy then
			if dy > 0 then Down else Up
		else
			if dx > 0 then Right else Left