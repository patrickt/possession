-- | 'Action' values are sent by a UI thread to the main ECS loop.
-- Each of these provokes some state change, except 'NoOp'.
-- They are dual to 'Command', which is sent from the ECS to the UI.
-- Thoughts: we could unify these two with a GADT.
module Game.Action
  ( Action (..),
  )
where

import Linear

data Action
  = Move (V2 Int)
  | NoOp
