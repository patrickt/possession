{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | 'Action' values are sent between the UI and game threads.
module Game.Action
  ( Action (..),
    Dest (..)
  )
where

import Linear
import Game.Canvas (Canvas)
import Game.Info (Info)
import Data.Message (Message)

data Dest = Game | UI

-- The type parameter here indicates the direction
-- in which this request can flow.
data Action (dest :: Dest) where
  Move :: V2 Int -> Action 'Game
  NoOp :: Action 'Game

  Redraw :: Canvas -> Action 'UI
  Update :: Info -> Action 'UI
  Notify :: Message -> Action 'UI
