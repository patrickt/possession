{-# LANGUAGE ImportQualifiedPost #-}
module Possession (main) where

import Brick qualified
import Brick.BChan
import Graphics.Vty qualified as Vty
import UI.App qualified as App

main :: IO ()
main = do
  brickEvLoop <- newBChan 1
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  Brick.customMain vty (pure vty) (Just brickEvLoop) App.app ()
