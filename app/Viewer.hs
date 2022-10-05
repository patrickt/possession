{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Game.Dungeon
import Control.Comonad
import Control.Monad
import Data.Vector.Universe (drawIO)

main :: IO ()
main = do
  let go (Dungeon x) = do
        drawIO x
        putStrLn "****"
        putStr "press enter to continue"
        void getLine
        go (Dungeon (extend step x))
  randomly >>= go
