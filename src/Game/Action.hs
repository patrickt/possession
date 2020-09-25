module Game.Action
  ( Action (..),
    Command (..),
  )
where

import Linear

data Action
  = Move (V2 Int)

data Command = Redraw
