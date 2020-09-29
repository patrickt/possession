{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.Modeline where

import Brick qualified
import Data.Text (Text)
import UI.Resource (Resource)

newtype Modeline = Modeline {unModeline :: Brick.Widget Resource}

modeline :: Modeline
modeline = Modeline (Brick.txt "hello")

update :: Modeline -> Text -> Modeline
update m t = m { unModeline = Brick.txt t }

render :: Modeline -> Brick.Widget Resource
render = unModeline
