{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.Widgets.Modeline
  ( Modeline,
    initial,
    messages,
    update,
    render,
  )
where

import Brick qualified
import Brick.Markup (markup, (@?))
import Brick.Widgets.List qualified as Brick
import Data.Message
import Data.Sequence (Seq)
import Data.Generics.Product
import Data.Text.Markup qualified as Markup
import Optics
import TextShow
import UI.Resource (Resource)
import UI.Resource qualified as Resource
import GHC.Generics (Generic)

newtype Modeline = Modeline (Seq Message)
  deriving Generic

messages :: Lens' Modeline (Seq Message)
messages = typed

initial :: Modeline
initial = Modeline mempty

update :: Message -> Modeline -> Modeline
update m (Modeline msgs) = Modeline (msgs |> m)

render :: Modeline -> Brick.Widget Resource
render (Modeline msgs) =
  Brick.viewport Resource.Modeline Brick.Vertical
    . Brick.vLimit 3
    . Brick.renderList (const renderMessage) False
    $ Brick.list Resource.Readout msgs 1

renderMessage :: Message -> Brick.Widget a
renderMessage m =
  let attr = case m ^. urgency of
        Info -> ""
        Warning -> "yellow"
        Danger -> "red"
      toAppend = case m ^. times of
        1 -> ""
        n -> " (" <> showt n <> "x)"
   in markup (((m ^. contents) @? attr) <> Markup.fromText toAppend)
