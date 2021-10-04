{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Hedgehog
import Control.Monad
import Gen qualified
import Data.Foldable
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.Monoid
import Data.Vector.Zipper qualified as Z

prop_zipperRotatesCancelOut :: Property
prop_zipperRotatesCancelOut = property do
  z <- forAll . Gen.zipper . Gen.vector (Range.linear 1 100) . Gen.int $ Range.linear 1 100
  times <- forAll . Gen.int $ Range.linear 0 50
  let make = fmap Endo . replicate times
  appEndo (fold (make Z.shiftLeft <> make Z.shiftRight)) z === z

prop_zipperRotateAllTheWayAround :: Property
prop_zipperRotateAllTheWayAround = property do
  z <- forAll . Gen.zipper . Gen.vector (Range.linear 1 100) . Gen.int $ Range.linear 1 100
  let shifts = replicate (length z) Z.shiftLeft
  appEndo (foldMap Endo shifts) z === z

main :: IO ()
main = void (checkParallel $$(discover))
