{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

data Mode
  = InMenu
  | InGame

data State = State
  { mode :: Mode,
    mainMenu :: MainMenu.State,
    canvas :: Game.Canvas,
    gamePort :: MVar Game.Action
  }
  deriving (Generic)

initial :: MVar Game.Action -> State
initial = State InMenu MainMenu.initial Canvas.empty

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
