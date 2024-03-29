{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Universe
  ( Univ (..),
    shiftRight,
    shiftLeft,
    shiftUp,
    shiftDown,
    neighbors,
    index,
    generateM,
    generate,
    drawIO,
  )
where

import Control.Comonad
import Control.DeepSeq
import Data.Coerce
import Data.Foldable.WithIndex
import Data.Functor.Identity
import Data.List (foldl')
import Data.Position qualified as P
import Data.Vector qualified as V (Vector, iterateN, reverse)
import Data.Vector.Zipper (Zipper (Zipper))
import Data.Vector.Zipper qualified as Z
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import Data.Position
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)

newtype Univ a = Univ { getZipper :: Zipper (Zipper a) }
  deriving stock (Eq, Functor, Foldable, Generic)
  deriving anyclass (NFData)

instance Show a => Show (Univ a) where
  show (Univ Z.Zipper {..}) = " " <> showLines before <> showCenterLine focus <> showLines after
    where
      showLines = foldl' (\str z -> str <> "\n" <> show z) ""
      showCenterLine x = "\n(" <> show x <> ")\n "

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
      innerLen = length (extract u)
      outerLen = length u
      before' = V.reverse (V.iterateN outerPos (fmap shiftUp) (fmap shiftUp focus'))
      after' = V.iterateN (outerLen - outerPos - 1) (fmap shiftDown) (fmap shiftDown focus')
      focus' =
        Zipper
          { Z.before = V.reverse (V.iterateN innerPos shiftLeft (shiftLeft parent)),
            Z.focus = parent,
            Z.after = V.iterateN (innerLen - innerPos - 1) shiftRight (shiftRight parent)
          }

instance FoldableWithIndex P.Position Univ where
  ifoldMap f (Univ a) = ifoldMap (\x -> ifoldMap (f . P.V2 x)) a

shiftRight, shiftLeft, shiftUp, shiftDown :: Univ a -> Univ a
shiftRight (Univ u) = Univ (fmap Z.shiftRight u)
shiftLeft (Univ u) = Univ (fmap Z.shiftLeft u)
shiftUp (Univ u) = Univ (Z.shiftLeft u)
shiftDown (Univ u) = Univ (Z.shiftRight u)

index :: Position -> Univ a -> a
index (x :- y) (Univ u) = Z.zindex x (Z.zindex y u)

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
generateM n f = Univ <$> Z.generateM n (\m -> Z.generateM n (f . P.V2 m))

generate :: Int -> (P.Position -> a) -> Univ a
generate n f = coerce (generateM n (Identity . f))

drawIO :: Pretty a => Univ a -> IO ()
drawIO = putDoc . pretty

instance Pretty a => Pretty (Univ a) where
  pretty (Univ g) = vcat . toList . fmap pretty . Z.toVector $ g
