{-# LANGUAGE ImportQualifiedPost #-}

module Game.Pathfind (module Game.Pathfind) where

import Data.Map.Strict qualified as Map
import Data.PQueue.Prio.Min qualified as PQueue
import Data.Position
import Game.Dungeon qualified as Dungeon
import Data.Maybe (fromMaybe)

type Path = [Position]

data PathError
  = NoRoute

type Heuristic = Position -> Double

type Scorer = Map.Map Position Double

type OpenSet = PQueue.MinPQueue Double Position
type CameFrom = Map.Map Position Position

score :: Position -> Scorer -> Double
score p = fromMaybe infinity . Map.lookup p

record :: Position -> Double -> Scorer -> Scorer
record = Map.insert

aStar :: Dungeon.Dungeon -> Position -> Position -> Heuristic -> Either PathError Path
aStar dungeon start goal heuristic = go (PQueue.singleton (heuristic start) start) mempty mempty
  where
    go ::
      OpenSet ->
      -- open-set, The set of discovered nodes that may need to be (re-)expanded.
      CameFrom ->
      -- came-from, maps a node to the one immediately preceding it on the cheapest currently-known path from start
      Scorer ->
      -- g-score, cost of the cheapest path from start to a node, currently known
      Either PathError Path
    go openSet cameFrom gScore
      | PQueue.null openSet = Left NoRoute
      | otherwise = do
        let ((_, current), newOpenSet) = PQueue.deleteFindMin openSet

        let reconstructNode :: [Position] -> Position -> [Position]
            reconstructNode acc it = case Map.lookup it cameFrom of
              Nothing -> acc
              Just a -> reconstructNode (a : acc) a


        let inner :: [Position] -> OpenSet -> CameFrom -> Scorer -> Either PathError Path
            inner neighbors openSet' cameFrom' gScore' = case neighbors of
              [] -> go openSet' cameFrom' gScore'
              neighbor : rest -> do
                let tentativeGScore = score current gScore' + edgeWeight current neighbor
                if tentativeGScore < score neighbor gScore'
                  then do
                    let newCameFrom = Map.insert neighbor current cameFrom
                    let newGScore = record neighbor tentativeGScore gScore'
                    let newFScore = score neighbor gScore' + heuristic neighbor
                    let newOpenSet'
                          | neighbor `elem` PQueue.elemsU openSet' = openSet'
                          | otherwise = PQueue.insert newFScore neighbor openSet'
                    inner rest newOpenSet' newCameFrom newGScore
                  else inner rest openSet' cameFrom' gScore'

        if current == goal
          then pure (reconstructNode [] current)
          else inner (fastNeighbors dungeon) newOpenSet cameFrom gScore

fastNeighbors :: Dungeon.Dungeon -> [Position]
fastNeighbors = error "not implemented"

edgeWeight :: Position -> Position -> Double
edgeWeight = error "not implemented"

infinity :: Double
infinity = read "Infinity"
