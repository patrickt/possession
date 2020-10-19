{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Color where

import Data.Either.Validation
import Data.Text (Text)
import Dhall qualified
import GHC.Generics (Generic)
import Graphics.Vty qualified as Vty

data Color = Black | Grey | White | Yellow | Brown
  deriving (Show, Generic)

toVty :: Color -> Vty.Color
toVty = \case
  Black -> Vty.black
  Grey -> Vty.rgbColor 221 221 (221 :: Int)
  White -> Vty.white
  Yellow -> Vty.brightYellow
  Brown -> Vty.rgbColor @Int 0x78 0x58 0x32

instance Dhall.FromDhall Color where
  autoWith n = Dhall.strictText {Dhall.extract = extract}
    where
      extract e = case Dhall.extract (Dhall.autoWith @Text n) e of
        Failure v -> Failure v
        Success t -> case t of
          "black" -> pure Black
          "grey" -> pure Grey
          "gray" -> pure Grey
          "white" -> pure White
          "yellow" -> pure Yellow
          "brown" -> pure Yellow
          x -> Dhall.extractError ("Unrecognized color name: " <> x)
