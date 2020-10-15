{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}

-- | A character used to hint the rendering system as to how to render
-- a given entity. We'll have to reexamine this when a GL background lands.
module Data.Glyph where

import Dhall qualified
import Data.Text (Text)
import Data.Text qualified as Text

newtype Glyph = Glyph Char deriving newtype (Show)

instance Dhall.FromDhall Glyph where
  autoWith n = fmap (Glyph . Text.head) (Dhall.autoWith n)
