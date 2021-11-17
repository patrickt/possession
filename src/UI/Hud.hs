{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module UI.Hud
  ( Hud (Hud),
  )
where

import Data.Message qualified as Message
import Data.Position (Position)
import Game.Info (Info)
import Optics
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline

newtype Hud = Hud
  { hudPosition :: Position
  }

makeFieldLabels ''Hud



insertReadout :: Position -> Info -> Modeline -> Modeline
insertReadout p i m = case i ^. #summary % at p of
  Just n -> m & Modeline.update (Message.youSee n)
  _ -> m
