module Data.Color
    ( Color (..)
    , toVty
    , settingTo
    ) where

import Raw.Types (Color (..))
import Graphics.Vty qualified as Vty

toVty :: Color -> Vty.Color
toVty = \case
  Black -> Vty.black
  Grey -> Vty.rgbColor @Int 221 221 221
  White -> Vty.white
  Red -> Vty.red
  Yellow -> Vty.brightYellow
  Green -> Vty.green
  Brown -> Vty.rgbColor @Int 0x78 0x58 0x32

settingTo :: Maybe Color -> Vty.MaybeDefault Vty.Color
settingTo = maybe Vty.Default (Vty.SetTo . toVty)
