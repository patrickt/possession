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
  let go x = do
        drawIO x
        putStrLn "****"
        void getLine
        go (extend step x)
  randomly >>= go
