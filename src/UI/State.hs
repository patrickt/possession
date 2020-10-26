{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module UI.State
  ( State (State),
    sidebar,
    mainMenu,
    mode,
    modeline,
    gamePort,
    canvas,
    gameThread,
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
import Optics
import UI.Input
import UI.MainMenu qualified as MainMenu
import UI.Sidebar (Sidebar)
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
    _canvas :: Game.Canvas,
    _modeline :: Modeline,
    _sidebar :: Sidebar,
    _gamePort :: MVar (Game.Action 'Game.Game),
    _gameThread :: ThreadId
  }
  deriving (Generic)

sidebar :: Lens' State Sidebar
sidebar = typed

mainMenu :: Lens' State MainMenu.State
mainMenu = typed

mode :: Lens' State Mode
mode = typed

modeline :: Lens' State Modeline
modeline = typed

gamePort :: Lens' State (MVar (Game.Action 'Game.Game))
gamePort = typed

canvas :: Lens' State Game.Canvas
canvas = typed

gameThread :: Lens' State ThreadId
gameThread = typed

initial :: MVar (Game.Action 'Game.Game) -> ThreadId -> State
initial gp tid =
  State
    { _mode = MainMenu,
      _mainMenu = MainMenu.initial,
      _canvas = Canvas.empty,
      _modeline = Modeline.initial,
      _sidebar = mempty @Sidebar,
      _gamePort = gp,
      _gameThread = tid
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
  (StartGame, MainMenu) -> s & mode .~ InGame
  (Menu, InGame) -> s & mode .~ MainMenu
  (Look, InGame) -> s & mode .~ Looking (s ^. sidebar % field @"info" % playerPosition % non 0)
  _ -> s
  where
    go x = s & selection %~ MainMenu.adjust x
    selection = mainMenu % field @"selected" % _Just

sendMaybe :: State -> Input -> Maybe State
sendMaybe _ Quit = Nothing
sendMaybe s i = Just (send i s)
