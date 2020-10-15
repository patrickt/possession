{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Amount (Amount (..)) where

import Dhall (FromDhall)
import Numeric.Natural
import Data.Word
import Data.Bifunctor
import System.Random.MWC (Variate (..))
import TextShow (TextShow)

newtype Amount = Amount {natural :: Natural}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, Real, Integral, Num, FromDhall, TextShow)

instance Variate Amount where
  uniform = fmap fromWord . uniform
  uniformR x g = fmap fromWord . flip uniformR g . bimap toWord toWord $ x

fromWord :: Word64 -> Amount
fromWord = fromIntegral

toWord :: Amount -> Word64
toWord = fromIntegral
