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
import Game.Info

data Toplevel a = Toplevel
  { toplevelCanvas :: Canvas (Toplevel a),
    toplevelSidebar :: Sidebar,
    toplevelModeline :: Modeline,
    toplevelParent :: a
  }
  deriving stock (Generic)

makeFieldLabels ''Toplevel

initial :: a -> Toplevel a
initial a = x where x = Toplevel (Canvas.initial x) Sidebar.initial Modeline.initial a

instance HasInfo a => HasInfo (Toplevel a) where
  info = #parent % info

instance HasInfo a => Responder (Toplevel a) where
  respondTo = within #canvas

instance Updateable Toplevel where
  update p t = go
   where
     go = t & #parent .~ p & #canvas %~ update go

instance Renderable (Toplevel a) where
  layout t =
    HSplit
      <$> (t ^. #sidebar % laidOut)
      <*> (VSplit <$> (t ^. #canvas % laidOut) <*> (t ^. #modeline % laidOut))
