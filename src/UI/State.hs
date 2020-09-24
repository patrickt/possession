{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module UI.State
  ( State (..),
    Mode (..),
    initial,
    send,
    sendMaybe,
  )
where

import UI.MainMenu qualified as MainMenu
import UI.Input
import Optics
import Data.Generics.Product.Fields
import GHC.Generics (Generic)

data Mode = InMenu

data State = State
  { mode :: Mode,
    mainMenu :: MainMenu.State
  } deriving Generic

initial :: State
initial = State InMenu MainMenu.initial

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
