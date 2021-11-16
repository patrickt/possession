{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module UI.Widgets.Toplevel
  ( Toplevel (Toplevel),
    initial,
  )
where

import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Core ((<=>), (<+>))
import Data.Maybe
import Data.Monoid
import GHC.Generics (Generic)
import Game.Action qualified as Action
import UI.Canvas (Canvas)
import UI.Canvas qualified as Canvas
import Graphics.Vty as Vty
import Linear (V2 (..))
import Optics
import UI.Hud qualified as Hud
import UI.Input qualified as Input
import UI.Widgets.MainMenu qualified as MainMenu
import UI.Render
import UI.Responder
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import UI.Widgets.Sidebar (Sidebar)
import UI.Widgets.Sidebar qualified as Sidebar

data Toplevel = Toplevel
  { toplevelCanvas :: Canvas,
    toplevelSidebar :: Sidebar,
    toplevelModeline :: Modeline
  }
  deriving stock (Generic)

makeFieldLabels ''Toplevel

initial :: Toplevel
initial =
  Toplevel Canvas.initial Sidebar.initial Modeline.initial

instance Responder Toplevel where
  respondTo _ _ = try #modeline <> try #canvas

instance Renderable Toplevel where
  layout t = HSplit (t ^. #sidebar % laidOut) (VSplit (t ^. #canvas % laidOut) (t ^. #modeline % laidOut))
