{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.Widgets.Modeline where

import Brick qualified
import Brick.Markup (markup, (@?))
import Brick.Widgets.List qualified as Brick
import Data.Message (Message (Message))
import Data.Message qualified as Message
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import UI.Resource (Resource)
import UI.Resource qualified as Resource

newtype Modeline = Modeline {contents :: Seq Message}

modeline :: Modeline
modeline = Modeline mempty

update :: Message -> Modeline -> Modeline
update m (Modeline msgs) = Modeline (msgs |> m)

render :: Modeline -> Brick.Widget Resource
render (Modeline msgs) =
  Brick.viewport Resource.Modeline Brick.Vertical
    . Brick.vLimit 3
    . Brick.renderList (const renderMessage) False
    $ Brick.list Resource.Readout msgs 1

renderMessage :: Message -> Brick.Widget a
renderMessage (Message msg urgency) =
  let attr = case urgency of
        Message.Info -> ""
        Message.Warning -> "yellow"
        Message.Danger -> "red"
   in markup (msg @? attr)
