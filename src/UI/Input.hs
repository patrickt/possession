{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module UI.Input
  ( Input (..),
    fromVty,
  ) where

import Graphics.Vty qualified as Vty

data Input = Up | Down | Quit | None

fromChar :: Char -> Maybe Input
fromChar = \case
  'q' -> pure Quit
  _ -> Nothing

fromVty :: Vty.Key -> Maybe Input
fromVty = \case
  Vty.KChar c -> fromChar c
  Vty.KUp -> Just Up
  Vty.KDown -> Just Down
  _ -> Nothing
