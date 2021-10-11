{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Optics
import UI.InGame qualified as InGame
import UI.MainMenu qualified as MainMenu
import UI.Responder.Chain qualified as Responder
import Prelude hiding (Either (..))

data State = State
  { stateResponders :: Responder.Chain,
    stateBrokerage :: Brokerage,
    stateGameThread :: ThreadId
  }
  deriving (Generic)

makeFieldLabels ''State

firstResponder :: Lens' State Responder.SomeResponder
firstResponder = typed % Responder.first

initial :: Brokerage -> ThreadId -> State
initial = State $ Responder.Chain
  [ Responder.SomeResponder MainMenu.initial,
    Responder.SomeResponder InGame.initial
  ]
