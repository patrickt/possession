{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

module UI.State
  ( State (State),
    mainMenu,
    mode,
    gamePort,
    gameThread,
    responders,
    Mode (..),
    _Looking,
    initial,
    send,
    sendMaybe,
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
import Game.Info (playerPosition)
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
    _mainMenu :: MainMenu.State,
    _game :: InGame.InGame,
    _gamePort :: MVar (Game.Action 'Game.Game),
    _gameThread :: ThreadId,
    _responders :: Responder.Chain
  }
  deriving (Generic)

responders :: Lens' State Responder.Chain
responders = typed

mainMenu :: Lens' State MainMenu.State
mainMenu = typed

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
      _mainMenu = MainMenu.initial,
      _game = InGame.initial,
      _gamePort = gp,
      _gameThread = tid,
      _responders = Responder.Chain [ Responder.SomeResponder MainMenu.initial
                                    , Responder.SomeResponder InGame.initial
                                    ]
    }

send :: Input -> State -> State
send i s = case (i, s ^. mode) of
  (Up, MainMenu) -> go Up
  (Down, MainMenu) -> go Down
  (Up, Looking _) ->
    s & mode % _Looking % _2 %~ pred
  (Down, Looking _) ->
    s & mode % _Looking % _2 %~ succ
  (Left, Looking _) ->
    s & mode % _Looking % _1 %~ pred
  (Right, Looking _) ->
    s & mode % _Looking % _1 %~ succ
  (Menu, InGame) -> s & mode .~ MainMenu
  -- (Look, InGame) -> s & mode .~ Looking (s ^. game % InGame.sidebar % field @"info" % playerPosition % non 0)
  _ -> s
  where
    go x = s & selection %~ MainMenu.adjust x
    selection = mainMenu % field @"selected" % _Just

sendMaybe :: State -> Input -> Maybe State
sendMaybe _ Quit = Nothing
sendMaybe s i = Just (send i s)
