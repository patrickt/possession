{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | A character used to hint the rendering system as to how to render
-- a given entity. We'll have to reexamine this when a GL backend lands.
module Data.Glyph where

import Data.Text qualified as Text
import Dhall qualified
import Data.Store (Store)

newtype Glyph = Glyph Char
  deriving newtype (Show, Store)

instance Dhall.FromDhall Glyph where
  autoWith n = fmap (Glyph . Text.head) (Dhall.autoWith n)
