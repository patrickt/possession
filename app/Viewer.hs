{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Game.Dungeon
import Control.Comonad
import Control.Monad

main :: IO ()
main = do
  let go x = do
        putStrLn (render x)
        putStrLn "****"
        void getLine
        go (extend step x)
  randomly >>= go
