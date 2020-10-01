{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Position
  ( Position (..)
  , V2 (..)
  , make
  , offset
  ) where

import Data.Ix
import Linear.V2

newtype Position = Position (V2 Int)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Ix, Num)

make :: Int -> Int -> Position
make x y = Position (V2 x y)

offset :: V2 Int -> Position -> Position
offset v (Position p) = Position (v + p)
