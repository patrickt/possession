{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level state container used by the Brick app.
module UI.State
  ( State (State),
    firstResponder,
    initial,
  )
where

import Control.Concurrent (ThreadId)
import Control.Effect.Broker (Brokerage)
import Data.Generics.Product
import GHC.Generics (Generic)
import Game.Info qualified as Game (Info)
import Optics
import UI.MainMenu qualified as MainMenu
import UI.Responder (CanHandle (..), Handler (..))
import UI.Responder.Chain qualified as Responder
import UI.Widgets.Toplevel qualified as Toplevel
import Prelude hiding (Either (..))

data State = State
  { stateLatestInfo :: Game.Info,
    stateToplevel :: Toplevel.Toplevel,
    stateMenu :: Maybe MainMenu.MainMenu,
    stateRenderStack :: Responder.Chain,
    stateBrokerage :: Brokerage,
    stateGameThread :: ThreadId
  }
  deriving (Generic)

makeFieldLabels ''State

instance CanHandle State where
  handleEvent _ _ = Try (#menu % _Just) <> Try #toplevel

firstResponder :: Lens' State Responder.SomeResponder
firstResponder = typed % Responder.first

initial :: Brokerage -> ThreadId -> State
initial =
  State mempty Toplevel.initial (Just MainMenu.initial) $
    Responder.Chain
      [ Responder.SomeResponder MainMenu.initial,
        Responder.SomeResponder Toplevel.initial
      ]
