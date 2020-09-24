{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module UI.MainMenu
  ( form,
    Choice (..),
    State (..),
    initial,
    adjust,
  )
where

import Brick qualified
import Brick.Forms qualified as Form
import Brick.Widgets.Border qualified as Brick
import Data.Generics.Product.Typed as Optics
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Optics.VL qualified as Optics
import UI.Resource
import UI.Input qualified as Input

data State = State {selected :: Maybe Choice}
  deriving (Generic)

initial :: State
initial = State Nothing

adjust :: Input.Input -> Maybe Choice -> Maybe Choice
adjust i s = case (i, s) of
  (Input.Up, Nothing) -> Just NewGame
  (Input.Down, Nothing) -> Just Quit
  (Input.Up, _) -> fmap moveUp s
  (Input.Down, _) -> fmap moveDown s
  _ -> s

data Choice
  = NewGame
  | Quit
  deriving (Eq, Ord, Show, Enum)

moveUp :: Choice -> Choice
moveUp = \case
  NewGame -> NewGame
  x -> pred x

moveDown :: Choice -> Choice
moveDown = \case
  Quit -> Quit
  x -> succ x

renderChoice :: Choice -> String
renderChoice = \case
  NewGame -> "New Game"
  Quit -> "Quit"

choices :: Vector Choice
choices = [NewGame, Quit]

render :: Bool -> Choice -> Brick.Widget n
render isOn =
  (if isOn then Brick.border else id)
    . Brick.str
    . renderChoice

form :: State -> Form.Form State e Resource
form = Form.newForm [theList]
  where
    theList =
      Form.listField
        (const choices)
        (Optics.toLensVL Optics.typed)
        render
        8
        Resource
