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
  (Up, InMenu) -> over adjuster (MainMenu.adjust Up) s
  (Down, _) -> over adjuster (MainMenu.adjust Down) s
  _ -> s
  where
    adjuster :: Lens' State (Maybe MainMenu.Choice)
    adjuster = field @"mainMenu" % field @"selected"

sendMaybe :: State -> Input -> Maybe State
sendMaybe _ Quit = Nothing
sendMaybe s i = Just (send i s)
