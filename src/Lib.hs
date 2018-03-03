module Lib (start, move) where

import Api

start :: StartReq -> StartResp
start _ = StartResp { color = "#FF0000", head_type = "pixel", tail_type = "pixel" }

move :: MoveReq -> MoveResp
move _ = MoveResp {}
