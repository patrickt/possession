module Main (main) where

import Gauge
import Game.Dungeon
import Data.Foldable
import Data.Monoid
import Control.Comonad

iter :: Dungeon -> Int -> Dungeon
iter g n = Dungeon . appEndo (fold (replicate n (Endo (extend step)))) . getDungeon $ g

main :: IO ()
main = do
  x <- randomly
  let y = iter x
  defaultMain [
    bgroup "fib" [ bench "10" $ nf y 20
                 , bench "35" $ nf y 40
                 , bench "37" $ nf y 60
                 ]
    ]
