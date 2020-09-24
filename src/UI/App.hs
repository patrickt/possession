{-# LANGUAGE ImportQualifiedPost #-}

module UI.App where

import Brick qualified
import Brick.AttrMap qualified as Brick.AttrMap
import Graphics.Vty qualified as Vty

ui :: Brick.Widget ()
ui = Brick.str "Possession, v0.0.0"

type State = ()

app :: Brick.App s e State
app =
  Brick.App
    { Brick.appDraw = const (pure ui),
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = Brick.resizeOrQuit,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = pure
    }
