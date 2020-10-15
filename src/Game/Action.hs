module Game.Action
  ( Action (..),
  )
where

import Linear

data Action
  = Move (V2 Int)
  | NoOp
