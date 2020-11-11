{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

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
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import UI.Responder qualified as Responder
import Optics
import UI.Input
import UI.MainMenu qualified as MainMenu
import UI.Sidebar (Sidebar)
import UI.InGame qualified as InGame
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import Prelude hiding (Either (..))

data Mode
  = MainMenu
  | InGame
  | Looking Position
  deriving (Generic)

_Looking :: Prism' Mode Position
_Looking = _Ctor @"Looking"

data State = State
  { _mode :: Mode,
    _gamePort :: MVar (Game.Action 'Game.Game),
    _gameThread :: ThreadId,
    _responders :: Responder.Chain
  }
  deriving (Generic)

responders :: Lens' State Responder.Chain
responders = typed

firstResponder :: Lens' State Responder.SomeResponder
firstResponder = typed % Responder.first

mode :: Lens' State Mode
mode = typed

gamePort :: Lens' State (MVar (Game.Action 'Game.Game))
gamePort = typed

gameThread :: Lens' State ThreadId
gameThread = typed

initial :: MVar (Game.Action 'Game.Game) -> ThreadId -> State
initial gp tid =
  State
    { _mode = MainMenu,
      _gamePort = gp,
      _gameThread = tid,
      _responders = Responder.Chain [ Responder.SomeResponder MainMenu.initial
                                    , Responder.SomeResponder InGame.initial
                                    ]
    }
