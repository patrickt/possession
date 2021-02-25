{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Vector.Universe qualified as U
import Data.Vector.Zipper qualified as Z
import Data.Monoid
import Data.Foldable
import Control.Comonad
import System.Random.Stateful (Uniform (..))
import Data.Vector qualified as V
import System.Random.Stateful qualified as R
import Data.Bool (bool)
import Control.Monad
import Debug.Trace

data Cell = Off | On deriving (Eq, Enum)

instance Uniform Cell where
  uniformM g = do
    x <- R.uniformRM @Int (1, 4) g
    pure (if x == 1 then On else Off)

instance Show Cell where
  show = \case
    On -> "#"
    Off -> "."

type Game = U.Univ Cell

renderIO :: Show a => U.Univ a -> IO ()
renderIO = putStrLn . render

render :: Show a => U.Univ a -> String
render (U.Univ g) = unlines cols
  where
    cols :: [String]
    cols = toList . fmap (foldMap show) . Z.toVector $ g

neighborCount :: Game -> Int
neighborCount = getSum . foldMap (Sum . fromEnum) . V.filter (== On) .  U.neighbors

randomly :: IO (U.Univ Cell)
randomly = do
  rand <- R.getStdGen >>= R.newIOGenM
  U.generateM 15 (const (uniformM rand))

step :: Game -> Cell
step g = cell
  where
    curr = extract g
    count = neighborCount g
    cell
      | count > 3 = Off
      | count < 2 = Off
      | curr == Off && count == 3 = On
      | otherwise = curr

main :: IO ()
main = do
  let go x = do
        putStrLn (render x)
        putStrLn "****"
        void getLine
        go (extend step x)
  randomly >>= go
