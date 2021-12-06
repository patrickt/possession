{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | 'Action' values are sent between the UI and game threads.
module Game.Action
  ( Action (..),
    Dest (..),
    GameAction,
    UIAction,
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
  -- These can be sent either way
  Start :: Action a
  Terminate :: Action a
  Notify :: Message -> Action a
  -- These are sent from UI to ECS
  Move :: V2 Int -> Action 'Game
  SaveState :: Action 'Game
  LoadState :: Action 'Game -- Should be Save -> Action Game
  -- These are sent from ECS to UI
  Redraw :: Canvas -> Action 'UI
  Update :: Info -> Action 'UI

instance Show (Action dest) where
  show = \case
    Start -> "Start"
    Terminate -> "Terminate"
    Move _ -> "Move"
    SaveState -> "SaveState"
    LoadState -> "LoadState"
    Redraw{} -> "Redraw"
    Update{} -> "Update"
    Notify{} -> "Notify"

type GameAction = Action 'Game
type UIAction = Action 'UI
