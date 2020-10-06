{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module UI.State
  ( State (..),
    Mode (..),
    initial,
    send,
    sendMaybe,
    broadcast,
  )
where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Generics.Product.Fields
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
  { mode :: Mode,
    mainMenu :: MainMenu.State,
    canvas :: Game.Canvas,
    modeline :: Modeline,
    sidebar :: Sidebar,
    gamePort :: MVar Game.Action
  }
  deriving (Generic)

initial :: MVar Game.Action -> State
initial gp =
  State
    { mode = InMenu,
      mainMenu = MainMenu.initial,
      canvas = Canvas.empty,
      modeline = Modeline.modeline,
      sidebar = Sidebar.initial,
      gamePort = gp
    }

broadcast :: MonadIO m => State -> Game.Action -> m ()
broadcast s act = liftIO . flip putMVar act . gamePort $ s

send :: Input -> State -> State
send i s = case (i, mode s, MainMenu.selected (mainMenu s)) of
  (Up, InMenu, _) -> go Up
  (Down, _, _) -> go Down
  (Accept, InMenu, Just MainMenu.NewGame) -> s & field @"mode" .~ InGame
  _ -> s
  where
    go x = over selection (MainMenu.adjust x) s
    selection = field @"mainMenu" % field @"selected" % _Just

sendMaybe :: State -> Input -> Maybe State
sendMaybe _ Quit = Nothing
sendMaybe s i = Just (send i s)
