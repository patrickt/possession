{-# LANGUAGE DeriveGeneric #-}

module Data.Color where

import GHC.Generics (Generic)

data Color = Black | Grey | White
  deriving (Show, Generic)
