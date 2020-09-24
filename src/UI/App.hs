{-# LANGUAGE ImportQualifiedPost #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified as Brick.AttrMap
import Graphics.Vty qualified as Vty
import UI.State qualified as UI (State)

ui :: Brick.Widget UI.State
ui = Brick.str "Possession, v0.0.0"

app :: Brick.App s e UI.State
app =
  Brick.App
    { Brick.appDraw = const (pure ui),
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = Brick.resizeOrQuit,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = pure
    }
