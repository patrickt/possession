{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Vector.Zipper
  ( Zipper (Zipper),
    index,
    toVector,
    make,
    makeM,
  )
where

import Data.Coerce
import Data.Functor.Identity
import Data.Vector (Vector)
import Data.Vector qualified as V

data Zipper a = Zipper
  { before :: Vector a,
    focus :: a,
    after :: Vector a
  }
  deriving stock (Functor) -- NB: we may want to parallelize this with `using` parTraversable

-- O(1)
toVector :: Zipper a -> Vector a
toVector Zipper{..} = V.concat [before, V.singleton focus, after]

-- O(1)
index :: Zipper a -> Int
index = V.length . before

-- O(n)
makeM :: forall m a. Monad m => Int -> (Int -> m a) -> m (Zipper a)
makeM n f =
  Zipper
    <$> V.generateM (mid - 1) f
    <*> f mid
    <*> V.generateM (n - mid + 1) go
    where
      go x = f (mid + x)
      mid = n `div` 2

-- O(n)
make :: forall a. Int -> (Int -> a) -> Zipper a
make = coerce (makeM @Identity @a)
