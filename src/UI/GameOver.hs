{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.GameOver (GameOver (..)) where

import Brick.Widgets.Center qualified as Brick (center)
import Brick qualified
import Graphics.Vty.Input qualified as Vty
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Render
import UI.Responder

data GameOver = GameOver

instance Responder GameOver where
  translate (Vty.EvKey Vty.KEnter _) _ = Input.Confirm
  translate _ _ = Input.None

  onSend Input.Confirm _ _ = Pop <> Push (SomeResponder MainMenu.initial)
  onSend _ _ _ = Nil

instance Renderable GameOver where
  render _ stack = Brick.center (Brick.txt "You have died. Press Enter to return to the main menu.") : stack
