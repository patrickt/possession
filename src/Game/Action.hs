{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | 'Action' values are sent between the UI and game threads.
module Game.Action
  ( Action (..),
    Dest (..),
  )
where

import Data.Message (Message)
import Game.Canvas (Canvas)
import Game.Info (Info)
import Linear

data Dest = Game | UI

-- The type parameter here indicates the direction
-- in which this request can flow. NoOp is bidirectional.
data Action (dest :: Dest) where
  NoOp :: Action a
  Move :: V2 Int -> Action 'Game
  Redraw :: Canvas -> Action 'UI
  Update :: Info -> Action 'UI
  Notify :: Message -> Action 'UI
