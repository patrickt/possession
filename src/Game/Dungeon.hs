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
  U.generateM 55 (const (uniformM rand))

birthLimit, deathLimit :: Int
birthLimit = 5
deathLimit = 3

step :: Game -> Cell
step g = cell
  where
    count = neighborCount g
    curr = extract g
    isOn = curr == On
    cell = if
      | isOn && count < deathLimit -> Off
      | isOn -> On
      | count > birthLimit -> On
      | otherwise -> Off
