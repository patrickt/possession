{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module UI.Responder
  ( Responder (..),
    try,
    recurse,
    runResponder,
    whenMatches,
    Alternative (..),
    guard,
    overState,
    respond,
    arr,
    (>>>),
    ensuring,
    within,
    tabulate,
    Res (..),
    switch,
    Updateable (..),
  )
where

import Control.Applicative
import Control.Arrow
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Category (Category)
import Data.Coerce
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Traversing
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import UI.Event
import Prelude hiding (id)

class Updateable f where
  update :: a -> f a -> f a

type ResponderM = StateC [GameAction] (NonDetC ((->) Vty.Event))

newtype Chain a b = Chain {unChain :: Star ResponderM a b}
  deriving newtype (Functor, Applicative, Alternative, Category, Profunctor, Strong, Traversing, Choice)

instance Arrow Chain where
  arr f = tabulate (pure . f)
  first = first'

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

switch fn = tabulate (\a -> case fn a of Ok x -> pure x; Fail -> empty @ResponderM; Emit e x -> x <$ modify (e :))

whenMatches :: (Is k A_Fold) => Optic' k is (Event a) x -> GameAction -> Chain a a
whenMatches opt act = ensuring (has opt) >>> switch (Emit act)

overState :: (Is k A_Fold) => Optic' k is (Event s) x -> (s -> s) -> Chain s s
overState opt fn = ensuring (has opt) >>> arr fn

ensuring :: (Event a -> Bool) -> Chain a a
ensuring fn = Chain (tabulate (\a -> ask >>= \e -> a <$ guard (fn (Event e a))))

runResponder :: Responder a => Vty.Event -> a -> Maybe ([GameAction], a)
runResponder e = ($ e) . runNonDetA . runState mempty . runChain respondTo

respond :: Responder b => Vty.Event -> b -> Maybe b
respond a = fmap snd . runResponder a
