{-# LANGUAGE ImportQualifiedPost #-}
module Gen
  ( zipper
  , vector
  ) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hedgehog.Range qualified as Range
import Data.Vector.Zipper (Zipper)
import Data.Vector.Zipper qualified as Z
import Control.Monad

zipper :: Gen (Vector a) -> Gen (Zipper a)
zipper v = do
  vec <- v
  guard (not $ Vector.null vec)
  Z.fromVector vec <$> Gen.int (Range.linear 0 (length vec))

vector :: Range Int -> Gen a -> Gen (Vector a)
vector r g = Vector.fromList <$> Gen.list r g
