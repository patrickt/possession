{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.SimpleResponder
  ( Responder (..),
    -- try,
    -- recurse,
    -- runResponder,
    -- whenMatches,
    -- Alternative (..),
    -- guard,
    -- emitting,
    -- overState,
    -- respond,
    -- upon,
    -- (>>>),
    -- ensuring,
    within,
    -- arr,
  )
where

import Control.Applicative
import Control.Carrier.Reader
import Control.Category (Category (..), (<<<), (>>>))
import Control.Monad
import Control.Monad.Free
import Data.Kind (Type)
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Traversing
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import UI.Event
import Prelude hiding ((.), id)
import Control.Carrier.NonDet.Church
import Data.Functor.Identity
import Control.Carrier.Throw.Either (ThrowC)

type Chain :: Type -> Type -> Type
newtype Chain a b = Chain {unChain :: Star Step a b}

runChain :: Chain a b -> a -> Step b
runChain (Chain (Star f)) = f

deriving newtype instance Category Chain
deriving newtype instance Profunctor Chain

class Responder a where
  respondTo :: Chain a a

instance Traversing Chain

type Step = ReaderC Vty.Event (NonDetC (ThrowC GameAction Identity))

instance Sieve Chain Step where

instance Representable Chain where
  type Rep Chain = ReaderC Vty.Event (NonDetC (ThrowC GameAction Identity))

withEvent :: Chain a Vty.Event
withEvent = tabulate (const ask)

recurse :: (Responder b, Is k A_Traversal) => Optic k is s t b b -> Chain s t
recurse setter = wander (traverseOf setter) respondTo

within ::
  ( JoinKinds A_Lens k1 k2,
    Responder b1,
    Is k2 A_Fold,
    Is k1 A_Traversal
  ) =>
  Optic k1 is b2 b2 b1 b1 ->
  Chain b2 b2
within setter = ensuring (has (#state % setter)) >>> recurse setter

ensuring :: (Event c -> Bool) -> Chain c c
ensuring fn = tabulate (\inp -> ask >>= \e -> inp <$ guard (fn (Event e inp)))

upon :: (a -> (Maybe GameAction, a)) -> Chain c c
upon = undefined

emitting it act = upon $ const (Just act, it)

whenMatches opt go = ensuring (has opt) >>> upon go

overState opt fn = whenMatches opt (pure . fn)

-- runResponder :: Responder a => Vty.Event -> a -> Maybe ([GameAction], a)
-- runResponder e = run . runReader e . runNonDetA . runState mempty . runKleisli respondTo

-- respond :: Responder b => Vty.Event -> b -> Maybe b
-- respond a = fmap snd . runResponder a
