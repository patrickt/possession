{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Responder
  ( Responder (..),
    try,
    Profunctor (..),
    recurse,
    runResponder,
    whenMatches,
    Alternative (..),
    guard,
    overState,
    respond,
    (>>>),
    ensuring,
    within,
    tabulate,
    Res (..),
    switch,
    andEmit,
    withEvent,
  )
where

import Control.Applicative
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Category
import Data.Coerce
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Traversing
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import UI.Event
import Prelude hiding (id, (.))

type ResponderM = StateC [GameAction] (NonDetC ((->) Vty.Event))

newtype Chain a b = Chain {unChain :: Star ResponderM a b}
  deriving newtype (Functor, Applicative, Alternative, Category, Profunctor, Strong, Traversing, Choice)

instance Sieve Chain ResponderM where
  sieve = coerce

instance Representable Chain where
  type Rep Chain = ResponderM
  tabulate = coerce

runChain :: Chain a b -> a -> ResponderM b
runChain = runStar . unChain

class Responder a where
  respondTo :: Chain a a

recurse :: (Responder b, Is k A_Traversal) => Optic k is s t b b -> Chain s t
recurse setter = wander (traverseOf setter) respondTo

try ::
  ( JoinKinds A_Lens l k,
    JoinKinds A_Lens k1 k2,
    Responder child,
    Is k A_Fold,
    Is k2 A_Fold,
    Is k1 A_Traversal
  ) =>
  Optic' l is1 parent ok ->
  Optic' k1 is2 parent child ->
  Chain parent parent
try getit setit = ensuring (has (#state % getit)) >>> within setit

within ::
  ( JoinKinds A_Lens k1 k2,
    Responder b1,
    Is k2 A_Fold,
    Is k1 A_Traversal
  ) =>
  Optic' k1 is b2 b1 ->
  Chain b2 b2
within setter = ensuring (has (#state % setter)) >>> recurse setter

data Res a = Ok a | Fail | Emit GameAction a

switch :: (t -> Res c) -> Chain t c
switch fn = tabulate (\a -> case fn a of Ok x -> pure x; Fail -> empty @ResponderM; Emit e x -> x <$ modify (e :))

whenMatches :: (Is k A_Fold) => Optic' k is (Event a) x -> GameAction -> Chain a a
whenMatches opt act = ensuring (has opt) >>> switch (Emit act)

overState :: (Is k A_Fold) => Optic' k is (Event s) x -> (s -> s) -> Chain s s
overState opt = flip rmap (ensuring (has opt))

withEvent :: forall a. Chain a (Event a)
withEvent = tabulate (\a -> Event <$> ask <*> pure a)

ensuring :: (Event a -> Bool) -> Chain a a
ensuring fn = go *> id where go = withEvent >>> tabulate (guard . fn)

andEmit :: (a -> GameAction) -> Chain a a
andEmit fn = tabulate (\a -> a <$ modify (fn a :))

runResponder :: Responder a => Vty.Event -> a -> Maybe ([GameAction], a)
runResponder e = ($ e) . runNonDetA . runState mempty . runChain respondTo

respond :: Responder b => Vty.Event -> b -> Maybe b
respond a = fmap snd . runResponder a
