{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module UI.Hud
  ( Hud (Hud),
    initial,
  )
where

import Data.Message qualified as Message
import Data.Position (Position, pattern (:-))
import Game.Info (Info)
import Optics
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty
import UI.Responder
import UI.Event (keypress)
import Data.Name

newtype Hud = Hud
  { hudPosition :: Position
  }
  deriving stock Generic

makeFieldLabels ''Hud

instance Responder Hud where
  respondTo =
    move Vty.KUp (0 :- negate 1)
    <|> move Vty.KDown (0 :- 1)
    <|> move Vty.KLeft (negate 1 :- 0)
    <|> move Vty.KRight (1 :- 0)
    where
      move k amt = overState (keypress k) (over #position (+amt) :: Hud -> Hud)

initial :: Hud
initial = Hud (3 :- 3)

insertReadout :: Position -> Info -> Modeline -> Modeline
insertReadout p i m = case i ^? #summary % ix p % name of
  Just n -> m & Modeline.update (Message.youSee n)
  _ -> m
