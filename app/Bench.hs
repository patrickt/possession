module Main (main) where

import Gauge
import Game.Dungeon
import Data.Monoid
import Control.Comonad
import Data.Foldable

iter :: Game -> Int -> Game
iter g n = appEndo (fold (replicate n (Endo (extend step)))) g

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
