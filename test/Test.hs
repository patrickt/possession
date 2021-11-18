{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Hedgehog
import Control.Monad
import Gen qualified
import Data.Foldable
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.Monoid
import Optics
import Optics.TH
import Data.Vector.Zipper qualified as Z
import UI.SimpleResponder

-- prop_zipperRotatesCancelOut :: Property
-- prop_zipperRotatesCancelOut = property do
--   z <- forAll . Gen.zipper . Gen.vector (Range.linear 1 100) . Gen.int $ Range.linear 1 100
--   times <- forAll . Gen.int $ Range.linear 0 50
--   let make = fmap Endo . replicate times
--   appEndo (fold (make Z.shiftLeft <> make Z.shiftRight)) z === z

-- prop_zipperRotateAllTheWayAround :: Property
-- prop_zipperRotateAllTheWayAround = property do
--   z <- forAll . Gen.zipper . Gen.vector (Range.linear 1 100) . Gen.int $ Range.linear 1 100
--   let shifts = replicate (length z) Z.shiftLeft
--   appEndo (foldMap Endo shifts) z === z

data ModalTest = ModalTest { menu :: MenuTest, canvas :: CanvasTest }
  deriving stock (Eq, Show)

data MenuTest = Closed | Open | Nil
  deriving stock (Eq, Show)

instance Responder MenuTest where
  respondTo Closed = pure Nil
  respondTo Open = pure Closed
  respondTo _ = empty

data CanvasTest = CanvasTest | Different
  deriving stock (Eq, Show)

instance Responder CanvasTest where
  respondTo = mempty

makeFieldLabelsNoPrefix ''ModalTest

instance Responder ModalTest where
  respondTo = descend #menu <> descend #canvas

prop_simpleResponderTestsWork :: Property
prop_simpleResponderTestsWork = withTests 1 $ property do
  run undefined Closed === Nil
  run undefined Open === Closed
  let mt = ModalTest Open CanvasTest
  let mt2 = run undefined mt
  mt2 === ModalTest Closed CanvasTest
  run undefined mt2 === ModalTest Nil Different

main :: IO ()
main = void (checkParallel $$(discover))
