{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level state container used by the Brick app.
module UI.State
  ( State (State),
    initial,
  )
where

import Control.Concurrent (ThreadId)
import Control.Effect.Broker (Brokerage)
import GHC.Generics (Generic)
import Game.Info qualified as Game (Info)
import Optics
import UI.Responder
import UI.Render
import qualified UI.Widgets.MainMenu as MainMenu
import qualified UI.Widgets.Toplevel as Toplevel

data State = State
  { stateToplevel :: Toplevel.Toplevel,
    stateMenu :: Maybe MainMenu.MainMenu,
    stateBrokerage :: Brokerage,
    stateGameThread :: ThreadId
  }
  deriving (Generic)

instance Show State where show = const "State"

makeFieldLabels ''State

instance Responder State where
  respondTo = try (#state % #menu % _Just) #menu <> recurse #toplevel

instance Renderable State where
  layout = Modal <$> preview (#menu % _Just % laidOut) <*> view (#toplevel % laidOut)

initial :: Brokerage -> ThreadId -> State
initial =
  State Toplevel.initial (Just MainMenu.initial)
