{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Game.World (World (..), initWorld) where

import Apecs
import Linear (V2 (..))

newtype Position = Position (V2 Double) deriving stock (Show)

newtype Glyph = Glyph Char deriving newtype (Show)

data Color = Black | Grey | White deriving (Show)

makeWorldAndComponents "World" [''Position, ''Glyph, ''Color]
