{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.State
  ( State (State),
    firstResponder,
    Mode (..),
    initial,
  )
where

import Control.Concurrent (ThreadId)
import Control.Effect.Broker (Brokerage)
import Data.Generics.Product
import Data.Position
import GHC.Generics (Generic)
import Optics
import UI.InGame qualified as InGame
import UI.MainMenu qualified as MainMenu
import UI.Responder.Chain qualified as Responder
import Prelude hiding (Either (..))

data Mode
  = MainMenu
  | InGame
  | Looking Position
  deriving (Generic)

data State = State
  { responders :: Responder.Chain,
    brokerage :: Brokerage,
    gameThread :: ThreadId
  }
  deriving (Generic)

makeFieldLabelsWith noPrefixFieldLabels ''State

firstResponder :: Lens' State Responder.SomeResponder
firstResponder = typed % Responder.first

initial :: Brokerage -> ThreadId -> State
initial = State $ Responder.Chain
  [ Responder.SomeResponder MainMenu.initial,
    Responder.SomeResponder InGame.initial
  ]
