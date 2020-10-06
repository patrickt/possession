{-# LANGUAGE DeriveGeneric #-}

module Data.Color where

import GHC.Generics (Generic)

data Color = Black | Grey | White | Yellow
  deriving (Show, Generic)
