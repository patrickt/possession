{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | A natural number representing a quantity of something.
-- Used for things like value of gold pieces.
module Data.Amount (Amount (..)) where

import Data.Bifunctor
import Data.Word
import Dhall qualified
import Numeric.Natural
import System.Random.MWC (Variate (..))
import TextShow (TextShow)

newtype Amount = Amount {natural :: Natural}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, Real, Integral, Num, Dhall.FromDhall, Dhall.ToDhall, TextShow)

instance Variate Amount where
  uniform = fmap fromWord . uniform
  uniformR x g = fmap fromWord . flip uniformR g . bimap toWord toWord $ x

fromWord :: Word64 -> Amount
fromWord = fromIntegral

toWord :: Amount -> Word64
toWord = fromIntegral
