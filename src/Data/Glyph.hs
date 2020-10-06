{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Glyph where

newtype Glyph = Glyph Char deriving newtype (Show)
