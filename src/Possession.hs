{-# LANGUAGE ImportQualifiedPost #-}
module Possession (main) where

import Brick qualified
import Brick.BChan
import Graphics.Vty qualified as Vty

ui :: Brick.Widget ()
ui = Brick.str "Possession, v0.0.0"

main :: IO ()
main = do
  brickEvLoop <- newBChan 1
  vty <- Vty.standardIOConfig >>= Vty.mkVty
  Brick.customMain vty (pure vty) (Just brickEvLoop) (Brick.simpleApp ui) ()
