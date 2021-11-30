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
    Alternative (..),
    guard,
    emitting,
    overState,
    respond,
    upon,
    (>>>),
    ensuring,
    within,
    arr,
  )
where

import Control.Applicative
import Prelude hiding (id)
import Control.Arrow
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import Optics.Arrow
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

try getit setit = ensuring (has (#state % getit)) >>> within setit

recurse setter = overA setter respondTo

within ::
  ( Algebra sig m,
    Responder b1,
    ArrowOptic k1 (Kleisli m),
    JoinKinds A_Lens k1 k2,
    Is k2 A_Fold,
    ResponderEff sig m
  ) =>
  Optic k1 is b b b1 b1 ->
  Kleisli m b b
within setter = ensuring (has (#state % setter)) >>> recurse setter

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
runResponder e = run . runReader e . runNonDetA . runState mempty . runKleisli respondTo

respond :: Responder b => Vty.Event -> b -> Maybe b
respond a = fmap snd . runResponder a
