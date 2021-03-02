{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Vector.Zipper
  ( module Data.Vector.Zipper,
  )
where

import Control.Comonad
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Functor.Identity
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Foldable.WithIndex
import Data.Functor.WithIndex.Instances ()
import GHC.Generics (Generic)

data Zipper a = Zipper
  { before :: Vector a,
    focus :: a,
    after :: Vector a
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- TODO: Check to see if this actually is meaningfully faster
instance Functor Zipper where
  fmap f Zipper {..} =
    Zipper
      { before = fmap f (before `using` parTraversable rseq),
        focus = f focus,
        after = fmap f (after `using` parTraversable rseq)
      }

instance Foldable Zipper where
  foldMap f Zipper {..} =
    mconcat
      [ foldMap f (before `using` parTraversable rseq),
        f focus,
        foldMap f (after `using` parTraversable rseq)
      ]

  length Zipper {..} = length before + 1 + length after


instance Comonad Zipper where
  -- O(1)
  extract = focus

  -- O(n)
  duplicate z@Zipper {..} =
    Zipper
      { before = before',
        focus = z,
        after = after'
      }
    where
      before' = Vector.reverse (Vector.iterateN (Vector.length before) shiftLeft (shiftLeft z))
      after' = Vector.iterateN (Vector.length after) shiftRight (shiftRight z)

instance FoldableWithIndex Int Zipper where
  ifoldMap f = ifoldMap f . toVector

focusIndex :: Zipper a -> Int
focusIndex = Vector.length . before

toVector :: Zipper a -> Vector a
toVector Zipper {..} = Vector.concat [before, Vector.singleton focus, after]

neighbors :: Zipper a -> Vector a
neighbors z = Vector.fromList [focus (shiftLeft z), focus (shiftRight z)]

-- ([], 1, [2, 3]
-- ([1, 2], 3, [])

shiftLeft :: Zipper a -> Zipper a
shiftLeft Zipper {..} = case Vector.unsnoc before of
  Nothing ->
    -- wrap around
    Zipper
      { before = Vector.cons focus (Vector.init after),
        focus = Vector.last after,
        after = Vector.empty
      }
  Just (init', last') ->
    Zipper
      { before = init',
        focus = last',
        after = Vector.cons focus after
      }

-- ([1, 2], 3, [])
-- ([], 1, [2, 3])

-- ([1,2], 3, [4, 5])
-- ([1,2,3], 4, [5])
shiftRight :: Zipper a -> Zipper a
shiftRight Zipper {..} = case Vector.uncons after of
  Nothing ->
    Zipper
      { before = Vector.empty,
        focus = Vector.head before,
        after = Vector.snoc (Vector.tail before) focus
      }
  Just (head', tail') ->
    Zipper
      { before = Vector.snoc before focus,
        focus = head',
        after = tail'
      }

generateM :: Monad m => Int -> (Int -> m a) -> m (Zipper a)
generateM count fn =
  let midpoint = count `div` 2
   in Zipper
        <$> Vector.generateM (midpoint - 1) fn
        <*> fn midpoint
        <*> Vector.generateM (count - midpoint - 1) (\x -> fn (x + midpoint))

generate :: Int -> (Int -> a) -> Zipper a
generate count fn = runIdentity (generateM count (Identity . fn))
