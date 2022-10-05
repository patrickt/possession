{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Vector.Zipper
  ( module Data.Vector.Zipper,
  )
where

import Control.Comonad
import Control.DeepSeq
import Data.Foldable.WithIndex
import Data.Functor.Identity
import Data.Functor.WithIndex.Instances ()
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Prettyprinter

data Zipper a = Zipper
  { before :: Vector a,
    focus :: a,
    after :: Vector a
  }
  deriving stock (Show, Eq, Generic, Functor, Foldable)
  deriving anyclass (NFData)

instance Pretty a => Pretty (Zipper a) where
  pretty (Zipper b f a) = foldMap pretty b <> pretty f <> foldMap pretty a

zindex :: Int -> Zipper a -> a
zindex n Zipper{..} =
  let leftlen = Vector.length before
      shifted = n - leftlen
  in if
    | n < leftlen -> before ! n
    | shifted == 0 -> focus
    | otherwise -> after ! (n - 1)

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

fromVector :: Vector a -> Int -> Zipper a
fromVector v idx
  | null v = error "Zipper.fromVector: tried to create a zipper over an empty vector"
  | length v == 1 = Zipper mempty (Vector.unsafeHead v) mempty
  | otherwise = let (fore, aft) = Vector.splitAt idx v in Zipper fore (v Vector.! idx) (Vector.drop 1 aft)

focusIndex :: Zipper a -> Int
focusIndex = Vector.length . before

toVector :: Zipper a -> Vector a
toVector Zipper {..} = Vector.concat [before, Vector.singleton focus, after]

neighbors :: Zipper a -> Vector a
neighbors z = Vector.fromList [focus (shiftLeft z), focus (shiftRight z)]

-- ([], 1, [2, 3]
-- ([1, 2], 3, [])

shiftLeft :: Zipper a -> Zipper a
shiftLeft z@Zipper {..}
  | singular z = z
  | otherwise = case Vector.unsnoc before of
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
shiftRight z@Zipper {..}
  | singular z = z
  | otherwise = case Vector.uncons after of
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

singular :: Zipper a -> Bool
singular (Zipper a _ b) = null a && null b
