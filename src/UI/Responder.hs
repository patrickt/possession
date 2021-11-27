{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Responder
  ( Responder (..),
    ResponderEff,
    try,
    recurse,
    runResponder,
    accept,
    whenMatches,
    respondingTo,
    Alternative (..),
    guard,
    emitting,
    overState,
    respond,
    upon,
    invoke,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import UI.Event

type ResponderEff sig m =
  ( Has (Reader Vty.Event) sig m,
    Has (State [GameAction]) sig m,
    Alternative m
  )

class Responder a where
  respondTo :: ResponderEff sig m => Kleisli m a a

upon :: (a -> m b) -> Kleisli m a b
upon = Kleisli

invoke :: Kleisli m a b -> a -> m b
invoke = runKleisli

accept :: Applicative f => a -> f a
accept = pure

emitting :: ResponderEff sig m => a -> GameAction -> m a
emitting it act = it <$ modify (act :)

respondingTo :: (ResponderEff sig m, Responder a) => a -> m a
respondingTo = runKleisli respondTo

try :: (ResponderEff sig m, Is k1 A_Traversal, Is k2 A_Fold, Responder b) => Optic' k2 is1 (Event s) a -> Optic' k1 is2 s b -> Kleisli m s s
try getit setit = Kleisli $ \a -> do
  evt <- ask
  guard (has getit (Event evt a))
  traverseOf setit respondingTo a

recurse :: (ResponderEff sig m, Is k A_Traversal, Responder b) => Optic' k is s b -> Kleisli m s s
recurse setter = Kleisli (traverseOf setter respondingTo)

whenMatches :: (ResponderEff sig m, Is k A_Fold) => Optic' k is (Event a) x -> (a -> m a) -> Kleisli m a a
whenMatches opt go = Kleisli $ \a -> do
  evt <- ask
  guard (has opt (Event evt a))
  go a

overState :: (ResponderEff sig m, Is k A_Fold) => Optic' k is (Event s) x -> (s -> s) -> Kleisli m s s
overState opt fn = whenMatches opt (pure . fn)

runResponder :: Responder a => Vty.Event -> a -> Maybe ([GameAction], a)
runResponder e = run . runReader e . runNonDetA . runState mempty . respondingTo

respond :: Responder b => Vty.Event -> b -> Maybe b
respond a = fmap snd . runResponder a
