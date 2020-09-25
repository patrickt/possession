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
import Optics
import UI.Input
import UI.MainMenu qualified as MainMenu

data Mode
  = InMenu
  | InGame

data State = State
  { mode :: Mode,
    mainMenu :: MainMenu.State,
    gamePort :: MVar Game.Action
  }
  deriving (Generic)

initial :: MVar Game.Action -> State
initial = State InMenu MainMenu.initial

broadcast :: MonadIO m => State -> Game.Action -> m ()
broadcast s act = liftIO . flip putMVar act . gamePort $ s

send :: Input -> State -> State
send i s = case (i, mode s) of
  (Up, InMenu) -> go Up
  (Down, _) -> go Down
  _ -> s
  where
    go x = over selection (MainMenu.adjust x) s
    selection = field @"mainMenu" % field @"selected" % _Just

sendMaybe :: State -> Input -> Maybe State
sendMaybe _ Quit = Nothing
sendMaybe s i = Just (send i s)
