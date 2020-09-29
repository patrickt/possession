module Game.Action
  ( Action (..),
    Command (..),
  )
where

import Game.Canvas (Canvas)
import Linear

data Action
  = Move (V2 Int)
  | NoOp

data Command = Redraw Canvas
