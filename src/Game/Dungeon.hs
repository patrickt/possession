{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Game.Dungeon where

import Control.Comonad
import Control.DeepSeq
import Data.Monoid
import Data.Vector qualified as V
import Data.Vector.Universe qualified as U
import GHC.Generics (Generic)
import System.Random.Stateful (Uniform (..))
import System.Random.Stateful qualified as R

data Cell = Off | On
  deriving stock (Eq, Enum, Generic)
  deriving anyclass (NFData)

instance Uniform Cell where
  uniformM g = do
    x <- R.uniformRM @Double (0, 1) g
    pure (if x <= 0.4 then On else Off)

instance Show Cell where
  show = \case
    On -> "#"
    Off -> "."

type Game = U.Univ Cell

neighborCount :: Game -> Int
neighborCount = getSum . foldMap (Sum . fromEnum) . V.filter (== On) . U.neighbors

randomly :: IO (U.Univ Cell)
randomly = do
  rand <- R.getStdGen >>= R.newIOGenM
  U.generateM 35 (const (uniformM rand))

step :: Game -> Cell
step g = result
  where
    -- Prevent death of cells with 5 or more neighbors
    deathLimit = 5
    -- Prevent birth of cells with 3 or fewer neighbors
    birthLimit = 3
    count = neighborCount g
    -- Is the current cell marked as on? (i.e. is there a wall there)
    isOn = extract g == On
    -- Truth table
    result = if
      | isOn && count < deathLimit -> Off
      | isOn -> On
      | count > birthLimit -> On
      | otherwise -> Off
