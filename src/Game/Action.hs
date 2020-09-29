module Game.Action
  ( Action (..),
    Command (..),
  )
where

import Linear
import Game.Canvas (Canvas)

data Action
  = Move (V2 Int)
  | NoOp

data Command = Redraw Canvas
