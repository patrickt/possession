{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.InGame where

import UI.Responder
import UI.Render
import Brick qualified

data InGame = InGame

initial :: InGame
initial = InGame

instance Responder InGame where
  onSend _ _ = Nil

instance Renderable InGame where
  render _ = Brick.txt "ingame"
