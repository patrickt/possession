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
import Optics
import UI.Resource
import UI.Input qualified as Input

data State = State {selected :: Maybe Choice}
  deriving (Generic)

initial :: State
initial = State (Just NewGame)

adjust :: Input.Input -> Choice -> Choice
adjust i s = case i of
  Input.Up -> moveUp s
  Input.Down -> moveDown s
  _ -> s

data Choice
  = NewGame
  | About
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
  x -> show x

choices :: Vector Choice
choices = [NewGame, About, Quit]

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