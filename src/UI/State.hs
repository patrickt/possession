{-# LANGUAGE ImportQualifiedPost #-}

module UI.State
  ( State (..),
    Mode (..),
    initial,
  )
where

import UI.MainMenu qualified as MainMenu

data Mode = InMenu

data State = State
  { mode :: Mode,
    mainMenu :: MainMenu.State
  }

initial :: State
initial = State InMenu MainMenu.initial
