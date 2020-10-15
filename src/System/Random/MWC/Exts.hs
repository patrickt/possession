{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides useful instances that MWC doesn't
module System.Random.MWC.Exts (Variate (..)) where

import System.Random.MWC
import Numeric.Natural
import Data.Word
import Data.Bifunctor

-- | Picks from Word64 values.
instance Variate Natural where
  uniform = fmap fromWord . uniform
  uniformR x g = fmap fromWord . flip uniformR g . bimap toWord toWord $ x

fromWord :: Word64 -> Natural
fromWord = fromIntegral

toWord :: Natural -> Word64
toWord = fromIntegral
