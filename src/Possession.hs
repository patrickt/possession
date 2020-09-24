{-# LANGUAGE ImportQualifiedPost #-}

module Possession (main) where

import Brick qualified

ui :: Brick.Widget ()
ui = Brick.str "Possession, v0.0.0"

main :: IO ()
main = Brick.simpleMain ui
