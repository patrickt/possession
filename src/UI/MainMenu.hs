{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.MainMenu
  ( MainMenu (MainMenu),
    initial,
    inGame,
  )
where

import Brick qualified
import Brick.Forms qualified as Form
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Data.List.Pointed (PointedList)
import Data.List.Pointed qualified as Pointed
import Data.String
import Game.Action (Action (SaveState, LoadState))
import GHC.Generics (Generic)
import GHC.Exts (fromList, toList)
import Graphics.Vty qualified as Vty
import Optics
import UI.Input qualified as Input
import UI.Render (Renderable (..))
import UI.Resource qualified as Resource
import UI.Responder

data Choice
  = NewGame
  | Resume
  | Load
  | Save
  | About
  | Quit
  deriving (Eq, Ord, Show, Enum)

instance Renderable Choice where
  render x _ =
    [ Brick.txt $ case x of
        NewGame -> "New Game"
        _ -> fromString (show x)
    ]

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

  onSend inp _inf s =
    case inp of
      Input.Up -> Update (s & #choices %~ Pointed.previous)
      Input.Down -> Update (s & #choices %~ Pointed.next)
      Input.Confirm -> case s ^. selected of
        NewGame -> Pop
        Quit -> Terminate
        Save -> Broadcast SaveState <> Pop
        Load -> Broadcast LoadState <> Pop
        Resume -> Pop
        _ -> Nil
      _ -> Nil

initial :: MainMenu
initial = MainMenu [NewGame, About, Quit]

inGame :: MainMenu
inGame = MainMenu [Resume, Save, Load, About, Quit]

render' :: Bool -> Choice -> Brick.Widget Resource.Resource
render' isOn =
  Brick.hCenter
    . (if isOn then Brick.border else id)
    . head
    . flip render []

form :: MainMenu -> Form.Form MainMenu e Resource.Resource
form = Form.newForm [theList]
  where
    theList =
      Form.listField
        (fromList . toList . view #choices)
        (toLensVL (selected % re (non NewGame)))
        render'
        8
        Resource.MainMenu

instance Renderable MainMenu where render s stack = Form.renderForm (form s) : stack
