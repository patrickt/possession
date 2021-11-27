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

import GHC.Generics (Generic)
import UI.Canvas (Canvas)
import UI.Canvas qualified as Canvas
import Optics
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

instance Show Toplevel where show = const "Toplevel"

makeFieldLabels ''Toplevel

initial :: Toplevel
initial =
  Toplevel Canvas.initial Sidebar.initial Modeline.initial

instance Responder Toplevel where
  respondTo = recurse #canvas

instance Renderable Toplevel where
  layout t = HSplit (t ^. #sidebar % laidOut) (VSplit (t ^. #canvas % laidOut) (t ^. #modeline % laidOut))
