{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.MainMenu
  ( form,
    Choice (..),
    State (..),
    initial,
  )
where

import Brick qualified
import Brick.Forms qualified as Form
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Data.Generics.Product.Typed as Optics (HasType (typed))
import Data.Vector
import GHC.Generics (Generic)
import Graphics.Vty qualified as Vty
import Optics
import UI.Input qualified as Input
import UI.Render (Renderable (..))
import UI.Resource qualified as Resource
import UI.Responder

data Choice
  = NewGame
  | About
  | Quit
  deriving (Eq, Ord, Show, Enum)

instance Renderable Choice where
  render =
    Brick.txt . \case
      NewGame -> "New Game"
      About -> "About"
      Quit -> "Quit"

newtype State = State
  { selected :: Maybe Choice
  }
  deriving (Generic)

makeFieldLabels ''State

instance Responder State where
  translate (Vty.EvKey k _) _ = case k of
    Vty.KUp -> Input.Up
    Vty.KDown -> Input.Down
    Vty.KEnter -> Input.Confirm
    _ -> Input.None
  translate _ _ = Input.None

  onSend i (State s) = case (i, s) of
    (Input.Up, Just NewGame) -> Nil
    (Input.Down, Just Quit) -> Nil
    (Input.Up, Just x) -> Update (State (Just (pred x)))
    (Input.Down, Just x) -> Update (State (Just (succ x)))
    (Input.Confirm, Just NewGame) -> Pop
    (Input.Confirm, Just Quit) -> Terminate
    _ -> Nil

initial :: State
initial = State (Just NewGame)

render' :: Bool -> Choice -> Brick.Widget Resource.Resource
render' isOn =
  Brick.hCenter
    . (if isOn then Brick.border else id)
    . render

form :: State -> Form.Form State e Resource.Resource
form = Form.newForm [theList]
  where
    theList =
      Form.listField
        (const [NewGame, About, Quit])
        (Optics.toLensVL Optics.typed)
        render'
        8
        Resource.MainMenu

instance Renderable State where render = Form.renderForm . form
