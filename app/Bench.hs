module Main (main) where

import Test.Tasty.Bench
import Game.Dungeon
import Data.Foldable
import Data.Monoid
import Control.Comonad
import System.Random

iter :: Dungeon -> Int -> Dungeon
iter g n = Dungeon . appEndo go . getDungeon $ g
  where
    go = fold (replicate n (Endo (extend step)))

main :: IO ()
main = do
  setStdGen (mkStdGen 12345)
  x <- randomly
  let y = iter x
  defaultMain [
    bgroup "fib" [ bench "20" $ nf y 20
                 , bench "40" $ nf y 40
                 , bench "60" $ nf y 60
                 ]
    ]
