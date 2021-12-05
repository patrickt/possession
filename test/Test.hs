{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Category (id)
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Vector.Zipper qualified as Z
import Gen qualified
import Graphics.Vty qualified as Vty
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Optics
import Optics.TH
import UI.Responder
import Prelude hiding (id)

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

data ModalTest = ModalTest {menu :: Maybe MenuTest, canvas :: CanvasTest}
  deriving stock (Eq, Show)

data MenuTest = Closed
  deriving stock (Eq, Show)

instance Responder MenuTest where
  respondTo = id

instance Responder (Maybe MenuTest) where
  respondTo = pure Nothing

data CanvasTest = CanvasTest | Different
  deriving stock (Eq, Show)

instance Responder CanvasTest where
  respondTo = tabulate \case
    CanvasTest -> pure Different
    Different -> pure CanvasTest

makePrisms ''MenuTest
makeFieldLabelsNoPrefix ''ModalTest

instance Responder ModalTest where
  respondTo = try (#menu % _Just) #menu <|> recurse #canvas

prop_simpleResponderTestsWork :: Property
prop_simpleResponderTestsWork = withTests 1 $ property do
  let someEvt = Vty.EvKey (Vty.KChar 'q') mempty
  let mt = ModalTest (Just Closed) CanvasTest
  let mt1 = respond someEvt mt
  let mt2 = mt1 >>= respond someEvt
  mt1 === Just (ModalTest Nothing CanvasTest)
  mt2 === Just (ModalTest Nothing Different)

main :: IO ()
main = void (checkParallel $$(discover))
