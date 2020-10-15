{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
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

data Input = Up | Down | Left | Right | Accept | Quit | Menu | None
  deriving (Show, Eq)

fromChar :: Char -> Maybe Input
fromChar = \case
  'q' -> pure Quit
  _ -> Nothing

-- we'll eventually need to move this into state, right?
-- we can't decide what the correct behavior of Enter is
-- unless we have access to the state, right?
fromVty :: Vty.Key -> [Vty.Modifier] -> Maybe Input
fromVty key mods = case key of
  Vty.KChar c -> case mods of
    [Vty.MCtrl] -> Just Quit
    _ -> fromChar c
  Vty.KUp -> Just Up
  Vty.KDown -> Just Down
  Vty.KLeft -> Just Left
  Vty.KRight -> Just Right
  Vty.KEnter -> Just Accept
  Vty.KEsc -> Just Menu
  _ -> Nothing

toAction :: Input -> Maybe (Game.Action 'Game.Game)
toAction i = case i of
  Up -> move (0, -1)
  Down -> move (0, 1)
  Left -> move (-1, 0)
  Right -> move (1, 0)
  _ -> Nothing
  where
    move (x, y) = Just (Game.Move (V2 x y))
