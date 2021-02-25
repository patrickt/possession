{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Vector.Universe
  ( Univ (..),
    shiftRight,
    shiftLeft,
    shiftUp,
    shiftDown,
    neighbors,
    generateM,
    generate,
  )
where

import Control.Comonad
import Data.Coerce
import Data.Functor.Identity
import Data.Position qualified as P
import Data.List (foldl')
import Data.Vector qualified as V
import Data.Vector.Zipper (Zipper (Zipper))
import Data.Vector.Zipper qualified as Z

newtype Univ a = Univ (Zipper (Zipper a))
  deriving (Eq, Functor)

instance Show a => Show (Univ a) where
  show (Univ Z.Zipper {..}) = " " <> showLines before <> showCenterLine focus <> showLines after
    where
      showLines l = foldl' (\str z -> str <> "\n" <> (show z)) "" l
      showCenterLine = \x -> "\n(" <> show x <> ")\n "

instance Comonad Univ where
  extract (Univ u) = extract (extract u)
  duplicate parent@(Univ u) =
    Univ $
      Zipper
        { Z.before = before',
          Z.focus = focus',
          Z.after = after'
        }
    where
      innerPos = Z.focusIndex (extract u)
      outerPos = Z.focusIndex u
      innerLen = Z.length (extract u)
      outerLen = Z.length u
      before' = V.reverse (V.iterateN outerPos (fmap shiftUp) (fmap shiftUp focus'))
      after' = V.iterateN (outerLen - outerPos - 1) (fmap shiftDown) (fmap shiftDown focus')
      focus' =
        Zipper
          { Z.before = V.reverse (V.iterateN innerPos shiftLeft (shiftLeft parent)),
            Z.focus = parent,
            Z.after = V.iterateN (innerLen - innerPos - 1) shiftRight (shiftRight parent)
          }

shiftRight, shiftLeft, shiftUp, shiftDown :: Univ a -> Univ a
shiftRight (Univ u) = Univ (fmap Z.shiftRight u)
shiftLeft (Univ u) = Univ (fmap Z.shiftLeft u)
shiftUp (Univ u) = Univ (Z.shiftLeft u)
shiftDown (Univ u) = Univ (Z.shiftRight u)

neighbors :: Univ a -> V.Vector a
neighbors u =
  [ go shiftUp,
    go (shiftUp . shiftRight),
    go shiftRight,
    go (shiftDown . shiftRight),
    go shiftDown,
    go (shiftDown . shiftLeft),
    go shiftLeft,
    go (shiftUp . shiftLeft)
  ]
  where
    go f = extract . f $ u

generateM :: Monad m => Int -> (P.Position -> m a) -> m (Univ a)
generateM n f = Univ <$> Z.generateM n (\m -> Z.generateM n (f . P.make m))

generate :: Int -> (P.Position -> a) -> Univ a
generate n f = coerce (generateM n (Identity . f))
