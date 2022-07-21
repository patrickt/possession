
{-# LANGUAGE ImportQualifiedPost #-}

module Game.Pathfind (module Game.Pathfind) where

import Data.Functor.Const
import Data.Functor.Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min qualified as PQueue
import Data.Position as Position
import GHC.Generics (Generic)
import Data.Functor.Identity
import Data.Coerce (coerce)

-- immutable components of a path search
data Context = Context
  { start :: Position,
    goal :: Position,
    heuristic :: Position -> Double,
    edgeWeight :: Position -> Position -> Double,
    neighborsFor :: Position -> [Position]
  }

-- mutable components
data St = St
  { openSet :: PQueue.MinPQueue Double Position,
    scorer :: Map.Map Position Double, -- TODO can we use histo to simulate this?
    best :: Position
  }
  deriving (Show)

makeState :: Context -> St
makeState ctx =
  St
    { openSet = PQueue.singleton 0 (start ctx),
      scorer = Map.singleton (start ctx) 0,
      best = start ctx
    }

data Prog m a
  = NoPath
  | IsDone (m Position)
  | InProgress (m Position) a
  deriving (Functor, Generic)

type instance Base (Prog m a) = Const (Prog m a)

instance Recursive (Prog m a) where project = Const

infinity :: Double
infinity = read "Infinity"

pathfind :: forall m . Monad m => Context -> m [Position]
pathfind c = hylo @(Prog m) retraceSteps (buildTree c) (makeState c)

pathfind' :: Context -> [Position]
pathfind' = coerce . pathfind @Identity

retraceSteps :: forall m . Applicative m => Prog m (m [Position]) -> m [Position]
retraceSteps = cata (go . getConst)
  where
    go = \case
      NoPath -> pure []
      IsDone pos -> fmap pure pos
      InProgress v ps -> (:) <$> v <*> ps

buildTree :: Monad m => Context -> St -> Prog m St
buildTree ctx s = case PQueue.minView (openSet s) of
  Nothing -> NoPath
  Just (current, newSet)
    | current == goal ctx -> IsDone (pure current)
    | otherwise -> InProgress (pure (best newState)) newState
    where
      newState = foldr adder s {openSet = newSet} . neighborsFor ctx $ current
      getScore p = fromMaybe infinity . Map.lookup p . scorer $ s
      adder :: Position -> St -> St
      adder neighbor st
        | tentativeScore < getScore neighbor =
          st
            { openSet = withNeighbor st,
              scorer = updated st,
              best = current
            }
        | otherwise = st
        where
          tentativeScore = edgeWeight ctx current neighbor + getScore current
          withNeighbor = PQueue.insert (tentativeScore + heuristic ctx neighbor) neighbor . openSet
          updated = Map.insert neighbor tentativeScore . scorer

-- pathfind :: Context -> [Position]
-- pathfind = hylo retraceSteps <$> buildTree <*> makeState

-- retraceSteps :: Prog [Position] -> [Position]
-- retraceSteps = cata (go . getConst)
--   where
--     go = \case
--       NoPath -> []
--       IsDone pos -> [pos]
--       InProgress v ps
--         | v == head ps -> ps
--         | otherwise -> v : ps

{-
.a...
.XXX.
.XX..
...b.
.....
-}

sampleContext :: Context
sampleContext =
  Context
    { start = 0 :- 1,
      goal = 4 :- 4,
      heuristic = Position.dist (goal sampleContext),
      edgeWeight = \_ _ -> 0,
      neighborsFor = \b ->
        [ x
          | b `notElem` [1 :- 1, 1 :- 2, 1 :- 3, 2 :- 1, 2 :- 2],
            x <- adjacentClamped 6 b
        ]
    }

sampleState :: St
sampleState = makeState sampleContext
