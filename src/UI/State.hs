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
    Mode (..),
    initial,
    send,
    sendMaybe,
  )
where

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
import UI.Sidebar qualified as Sidebar
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline

data Mode
  = InMenu
  | InGame

data State = State
  { _mode :: Mode,
    _mainMenu :: MainMenu.State,
    _canvas :: Game.Canvas,
    _modeline :: Modeline,
    _sidebar :: Sidebar,
    _gamePort :: MVar Game.Action
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

gamePort :: Lens' State (MVar Game.Action)
gamePort = typed

canvas :: Lens' State Game.Canvas
canvas = typed

initial :: MVar Game.Action -> State
initial gp =
  State
    { _mode = InMenu,
      _mainMenu = MainMenu.initial,
      _canvas = Canvas.empty,
      _modeline = Modeline.initial,
      _sidebar = Sidebar.initial,
      _gamePort = gp
    }

send :: Input -> State -> State
send i s = case (i, s ^. mode, s ^. mainMenu % field @"selected") of
  (Up, InMenu, _) -> go Up
  (Down, _, _) -> go Down
  (Accept, InMenu, Just MainMenu.NewGame) -> s & mode .~ InGame
  _ -> s
  where
    go x = s & selection %~ MainMenu.adjust x
    selection = mainMenu % field @"selected" % _Just

sendMaybe :: State -> Input -> Maybe State
sendMaybe _ Quit = Nothing
sendMaybe s i = Just (send i s)
