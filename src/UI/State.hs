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
    mode,
    gamePort,
    gameThread,
    responders,
    firstResponder,
    Mode (..),
    _Looking,
    initial,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Data.Generics.Product
import Data.Generics.Sum
import Data.Position
import GHC.Generics (Generic)
import Game.Action qualified as Game
import Optics
import UI.InGame qualified as InGame
import UI.MainMenu qualified as MainMenu
import UI.Responder qualified as Responder
import Prelude hiding (Either (..))

data Mode
  = MainMenu
  | InGame
  | Looking Position
  deriving (Generic)

_Looking :: Prism' Mode Position
_Looking = _Ctor @"Looking"

data State = State
  { mode :: Mode,
    gamePort :: MVar (Game.Action 'Game.Game),
    gameThread :: ThreadId,
    responders :: Responder.Chain
  }
  deriving (Generic)

firstResponder :: Lens' State Responder.SomeResponder
firstResponder = typed % Responder.first

makeFieldLabelsWith noPrefixFieldLabels ''State

initial :: MVar (Game.Action 'Game.Game) -> ThreadId -> State
initial gp tid =
  let chain =
        Responder.Chain
          [ Responder.SomeResponder MainMenu.initial,
            Responder.SomeResponder InGame.initial
          ]
   in State MainMenu gp tid chain
