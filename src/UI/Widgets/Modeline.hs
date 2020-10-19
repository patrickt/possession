{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module UI.Widgets.Modeline
  ( Modeline,
    initial,
    messages,
    update,
    render,
  )
where

import Brick qualified
import Brick.Widgets.List qualified as Brick
import Data.Generics.Product
import Data.Message
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import Optics
import UI.Render
import UI.Resource qualified as Resource

newtype Modeline = Modeline (Seq Message)
  deriving (Generic)

messages :: Lens' Modeline (Seq Message)
messages = typed

initial :: Modeline
initial = Modeline mempty

update :: Message -> Modeline -> Modeline
update m (Modeline msgs) = Modeline (msgs |> m)

instance Renderable Modeline where
  render (Modeline msgs) =
    Brick.viewport Resource.Modeline Brick.Vertical
      . Brick.vLimit 3
      . Brick.renderList (const (render @Message)) False
      -- TODO: move this viewport appropriately rather than gyrating with drop
      $ Brick.list Resource.Readout (Seq.drop (length msgs - 3) msgs) 1
