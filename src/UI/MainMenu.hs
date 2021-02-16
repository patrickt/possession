{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.MainMenu
  ( MainMenu (MainMenu),
    initial,
  )
where

import Brick qualified
import Brick.Forms qualified as Form
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Center qualified as Brick
import GHC.Generics (Generic)
import Graphics.Vty qualified as Vty
import Optics
import UI.Input qualified as Input
import UI.Render (Renderable (..))
import UI.Resource qualified as Resource
import UI.Responder
import Data.List.Pointed (PointedList)
import Data.List.Pointed qualified as Pointed

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

newtype MainMenu = MainMenu
  { choices :: PointedList Choice
  }
  deriving (Generic)

makeFieldLabelsWith noPrefixFieldLabels ''MainMenu

selected :: Lens' MainMenu Choice
selected = #choices % Pointed.focus

instance Responder MainMenu where
  translate (Vty.EvKey k _) _ = case k of
    Vty.KUp -> Input.Up
    Vty.KDown -> Input.Down
    Vty.KEnter -> Input.Confirm
    _ -> Input.None
  translate _ _ = Input.None

  onSend i s =
    case i of
      Input.Up -> Update (s & #choices %~ Pointed.previous)
      Input.Down -> Update (s & #choices %~ Pointed.next)
      Input.Confirm
        | s ^. selected == NewGame -> Pop
        | s ^. selected == Quit -> Terminate
        | otherwise -> Nil
      _ -> Nil

initial :: MainMenu
initial = MainMenu [NewGame, About, Quit]

render' :: Bool -> Choice -> Brick.Widget Resource.Resource
render' isOn =
  Brick.hCenter
    . (if isOn then Brick.border else id)
    . render

form :: MainMenu -> Form.Form MainMenu e Resource.Resource
form = Form.newForm [theList]
  where
    setSelected :: MainMenu -> Maybe Choice -> MainMenu
    setSelected m c
      = maybe (m & #choices %~ Pointed.moveTo 0) (\v -> set selected v m) c
    theList =
      Form.listField
        (const [NewGame, About, Quit])
        (toLensVL (Optics.lens (Just . view selected) setSelected))
        render'
        8
        Resource.MainMenu

instance Renderable MainMenu where render = Form.renderForm . form
