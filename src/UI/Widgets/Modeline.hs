{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Brick.Widgets.Center qualified as Brick
import Data.Message
import Data.Monoid.Generic
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import Optics
import UI.Render
import UI.Resource qualified as Resource

newtype Modeline = Modeline
  { messages :: Seq Message
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Modeline
  deriving (Monoid) via GenericMonoid Modeline

makeFieldLabelsWith noPrefixFieldLabels ''Modeline

initial :: Modeline
initial = mempty

update :: Message -> Modeline -> Modeline
update m (Modeline msgs) = Modeline (msgs |> m)

instance Renderable Modeline where
  render (Modeline msgs) stack =
    let readout =
          Brick.hCenter
          . Brick.renderList (const (head . flip (render @Message) [])) False
          -- TODO: move this viewport appropriately rather than gyrating with drop
          $
            Brick.list Resource.Readout (Seq.drop (length msgs - 3) msgs) 1
        final = Brick.vLimit 3
                . Brick.viewport Resource.Modeline Brick.Vertical
                . Brick.vLimit 3
                $ readout
     in final : stack
