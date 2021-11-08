{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Game.Dungeon
  ( Dungeon (..)
  , Cell (..)
  , step
  , at
  , randomly
  , makeDungeon
  ) where

import Control.Comonad
import Control.DeepSeq
import Data.Monoid
import Data.Vector qualified as V
import Data.Vector.Universe qualified as U
import GHC.Generics (Generic)
import System.Random.Stateful (Uniform (..))
import System.Random.Stateful qualified as R
import Data.Semigroup (stimes)
import Prettyprinter qualified as Pretty
import Data.Foldable
import Data.Position (Position)

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

instance Pretty.Pretty Cell where
  pretty = Pretty.viaShow

newtype Dungeon = Dungeon { getDungeon :: Game }
  deriving newtype NFData

type Game = U.Univ Cell

neighborCount :: Game -> Int
neighborCount = length . V.filter (== On) . U.neighbors

randomly :: IO Dungeon
randomly = do
  rand <- R.getStdGen >>= R.newIOGenM
  Dungeon <$> U.generateM 60 (const (uniformM rand))

at :: Dungeon -> Position -> Cell
at (Dungeon d) p = U.index p d

step :: Game -> Cell
step g = result
  where
    -- Prevent death of cells with 5 or more neighbors
    deathLimit = 4
    -- Prevent birth of cells with 3 or fewer neighbors
    birthLimit = 3
    count = neighborCount g
    -- Is the current cell marked as on? (i.e. is there a wall there)
    isOn = extract g == On
    -- Truth table
    result =
      if
          | isOn && count < deathLimit -> Off
          | isOn -> On
          | count > birthLimit -> On
          | otherwise -> Off

makeDungeon :: IO Dungeon
makeDungeon = do
  Dungeon start <- randomly
  let iter = stimes (3 :: Int) (Endo (extend step))
  pure . Dungeon $ iter `appEndo` start

instance Pretty.Pretty Dungeon where
  pretty (Dungeon d) = Pretty.vcat . fmap Pretty.pretty . toList . U.getZipper $ d
