{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module UI.Input
  ( Input (..),
    fromVty,
    toAction,
  )
where

import Game.Action qualified as Game
import Graphics.Vty qualified as Vty
import Linear
import Prelude hiding (Either (..))

data Input = Up | Down | Left | Right | Quit | None

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

toAction :: Input -> Maybe Game.Action
toAction i = case i of
  Up -> move (0, -1)
  Down -> move (0, 1)
  Left -> move (-1, 0)
  Right -> move (1, 0)
  _ -> Nothing
  where
    move (x, y) = Just (Game.Move (V2 x y))