{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Responder
  ( Responder (..),
    try,
    recurse,
    runResponder,
    whenMatches,
    respondingTo,
    Alternative (..),
    guard,
    emitting,
    overState,
    respond,
    upon,
    (>>>),
    ensuring,
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

emitting :: ResponderEff sig m => a -> GameAction -> m a
emitting it act = it <$ modify (act :)

respondingTo :: (ResponderEff sig m, Responder a) => a -> m a
respondingTo = runKleisli respondTo

try :: (Algebra sig m, Is k1 A_Traversal, Is k2 A_Fold, JoinKinds A_Lens l k2, ResponderEff sig m, Responder b1) => Optic l is1 b2 b2 a a -> Optic k1 is2 b2 c b1 b1 -> Kleisli m b2 c
try getit setit = ensuring (has (#state % getit)) >>> upon (traverseOf setit respondingTo)

recurse :: (ResponderEff sig m, Is k A_Traversal, Responder b) => Optic' k is s b -> Kleisli m s s
recurse setter = upon (traverseOf setter respondingTo)

whenMatches :: (ResponderEff sig m, Is k A_Fold) => Optic' k is (Event a) x -> (a -> m a) -> Kleisli m a a
whenMatches opt go = ensuring (has opt) >>> upon go

overState :: (ResponderEff sig m, Is k A_Fold) => Optic' k is (Event s) x -> (s -> s) -> Kleisli m s s
overState opt fn = whenMatches opt (pure . fn)

ensuring :: ResponderEff sig m => (Event a -> Bool) -> Kleisli m a a
ensuring fn = Kleisli $ \a -> do
  evt <- ask
  guard (fn (Event evt a))
  pure a

runResponder :: Responder a => Vty.Event -> a -> Maybe ([GameAction], a)
runResponder e = run . runReader e . runNonDetA . runState mempty . respondingTo

respond :: Responder b => Vty.Event -> b -> Maybe b
respond a = fmap snd . runResponder a
