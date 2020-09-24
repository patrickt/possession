{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module UI.MainMenu
  ( form,
    Choice (..),
    State,
    initial,
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

data State = State {_selected :: Maybe Choice}
  deriving (Generic)

initial :: State
initial = State Nothing

data Choice
  = NewGame
  | Quit
  deriving (Eq, Ord, Show)

renderChoice :: Choice -> String
renderChoice = \case
  NewGame -> "New Game"
  Quit -> "Quit"

choices :: Vector Choice
choices = [NewGame, Quit]

render :: Bool -> Choice -> Brick.Widget n
render _selected =
  Brick.border
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
