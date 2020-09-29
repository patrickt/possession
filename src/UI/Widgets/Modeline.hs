{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.Modeline where

import Brick qualified
import Data.Text (Text)
import UI.Resource (Resource)
import UI.Resource qualified as Resource

newtype Modeline = Modeline {unModeline :: Brick.Widget Resource}

modeline :: Modeline
modeline = Modeline (Brick.txt "hello")

update :: Modeline -> Text -> Modeline
update m t = m { unModeline = Brick.txt t }

render :: Modeline -> Brick.Widget Resource
render = Brick.vLimit 3 . Brick.viewport Resource.Modeline Brick.Vertical . unModeline
