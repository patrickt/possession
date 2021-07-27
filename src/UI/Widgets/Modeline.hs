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
import Data.Color (Color)
import Data.Foldable (toList)
import Data.Message
import Data.Monoid.Generic
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics
import UI.Render
import UI.Resource qualified as Resource

data Mode = Mode Text Color

instance Renderable Mode where
  render (Mode t c) = withForeground c (Brick.txt t)

data Modeline = Modeline
  { messages :: Seq Message,
    modes :: Seq Mode
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Modeline
  deriving (Monoid) via GenericMonoid Modeline

makeFieldLabelsWith noPrefixFieldLabels ''Modeline

initial :: Modeline
initial = mempty

update :: Message -> Modeline -> Modeline
update m (Modeline msgs mods) = Modeline (msgs |> m) mods

instance Renderable Modeline where
  render (Modeline msgs mods) =
    let readout =
          Brick.renderList (const (render @Message)) False
          -- TODO: move this viewport appropriately rather than gyrating with drop
          $
            Brick.list Resource.Readout (Seq.drop (length msgs - 3) msgs) 1
        badges =
          toList (fmap render mods)
     in Brick.vLimit 3
          . Brick.viewport Resource.Modeline Brick.Vertical
          . Brick.vLimit 3
          $ Brick.hBox (readout : badges)
