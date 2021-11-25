{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module UI.Responder
  ( Responder (..),
    ResponderEff,
    try,
    recurse,
    runResponder,
    accept,
    whenMatches,
    Alternative (..),
    guard,
  emitting,overState,respond)
where

import Data.Kind (Type)
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import UI.Event
import Control.Applicative
import Control.Arrow
import Data.Monoid (First (..))
import Data.Monoid qualified as Monoid
import Data.Semigroup qualified as Semigroup
import Data.Fix (Fix (..))
import Control.Monad
import Control.Carrier.State.Strict
import Control.Carrier.Reader
import Control.Carrier.NonDet.Church
import Data.Functor.Foldable
import Data.Bool (bool)

type ResponderEff sig m =
  ( Has (Reader Vty.Event) sig m
  , Has (State [GameAction]) sig m
  , Alternative m)

class Responder a where
  respondTo :: ResponderEff sig m => a -> m a

accept :: Applicative f => a -> f a
accept = pure

emitting :: ResponderEff sig m => a -> GameAction -> m a
emitting it act = it <$ modify (act:)

respondingTo :: (ResponderEff sig m, Responder a) => a -> m a
respondingTo = respondTo

try :: (ResponderEff sig m, Is k1 A_Traversal, Is k2 A_Fold, Responder b) => Optic' k2 is1 (Event s) a -> Optic' k1 is2 s b -> s -> m s
try getit setit a = do
  evt <- ask
  recurse setit a <* guard (has getit (Event evt a))

recurse :: (ResponderEff sig m, Is k A_Traversal, Responder b) => Optic' k is s b -> s -> m s
recurse setter = traverseOf setter respondingTo

whenMatches :: (ResponderEff sig m, Is k A_Fold) => Optic' k is (Event a) x -> (a -> m a) -> a -> m a
whenMatches opt go a = do
  evt <- ask
  guard (has opt (Event evt a))
  go a

overState :: (ResponderEff sig m, Is k A_Fold) => Optic' k is (Event s) x -> (s -> s) -> s -> m s
overState opt fn a = do
  evt <- ask
  guard (has opt (Event evt a))
  pure (fn a)


runResponder :: Responder a => Vty.Event -> a -> Maybe ([GameAction], a)
runResponder e  = run . runReader e . runNonDetA . runState mempty . respondTo

respond a = fmap snd . runResponder a
