module Game.Action
  ( Action (..),
  )
where

import Game.Canvas (Canvas)
import Linear

data Action
  = Move (V2 Int)
  | NoOp
