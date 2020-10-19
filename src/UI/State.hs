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

import Prelude hiding (Either (..))
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar
import Data.Generics.Product
import GHC.Generics (Generic)
import Game.Action qualified as Game
import Game.Canvas qualified as Canvas
import Game.Canvas qualified as Game (Canvas)
import Optics
import UI.Input
import UI.MainMenu qualified as MainMenu
import UI.Sidebar (Sidebar)
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import Data.Position
import Data.Generics.Sum
import Game.Info (playerPosition)

data Mode
  = MainMenu
  | InGame
  | Looking Position
    deriving Generic

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
send i s = case (i, s ^. mode, s ^. mainMenu % field @"selected") of
  (Up, MainMenu, _) -> go Up
  (Down, MainMenu, _) -> go Down
  (Up, Looking _, _) ->
    s & mode % _Ctor @"Looking" % _2 %~ pred
  (Down, Looking _, _) ->
    s & mode % _Ctor @"Looking" % _2 %~ succ
  (Left, Looking _, _) ->
    s & mode % _Ctor @"Looking" % _1 %~ pred
  (Right, Looking _, _) ->
    s & mode % _Ctor @"Looking" % _1 %~ succ
  (Accept, MainMenu, Just MainMenu.NewGame) -> s & mode .~ InGame
  (Menu, InGame, _) -> s & mode .~ MainMenu
  (Look, InGame, _) -> s & mode .~ Looking (s ^. sidebar % field @"info" % playerPosition % non 0)
  _ -> s
  where
    go x = s & selection %~ MainMenu.adjust x
    selection = mainMenu % field @"selected" % _Just

sendMaybe :: State -> Input -> Maybe State
sendMaybe _ Quit = Nothing
sendMaybe s i = Just (send i s)
