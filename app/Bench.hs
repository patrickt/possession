module Main (main) where

import Test.Tasty.Bench
import Game.Dungeon
import Data.Monoid
import Control.Comonad
import Data.Foldable

iter :: Game -> Int -> Game
iter g n = appEndo (fold (replicate n (Endo (extend step)))) g

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

myFibo :: Int -> Integer
myFibo n = if n < 3 then toInteger n else myFibo (n - 1) + myFibo (n - 2)

main :: IO ()
main = do
  x <- randomly
  Test.Tasty.Bench.defaultMain -- not Test.Tasty.defaultMain
    [ bench "iter 20" $ nf (iter x) 20
    , bench "iter 40" $ nf (iter x) 40
    , bench "iter 60" $ nf (iter x) 60
    ]
