{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Responder
  ( Path (..),
    Responder (..),
    Event (..),
    whenMatches,
    emitting,
    overState,
    respond,
    try,
    recurse,
    eval,
    switch,
    result,
    emptyResult,
  )
where

import Data.Kind (Type)
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import UI.Event
import Control.Alternative.Free
import Control.Applicative
import Control.Arrow
import Data.Monoid (First (..))
import Data.Monoid qualified as Monoid
import Control.Monad

data Result a = Ok a | Fail | Emit [GameAction] a
  deriving stock Functor

instance Semigroup (Result a) where
  Fail <> a = a
  a <> _ = a

instance Monoid (Result a) where mempty = Fail

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok a = Ok (f a)
  Ok f <*> Emit e a = Emit e (f a)
  Emit e f <*> Ok a = Emit e (f a)
  Emit e f <*> Emit e' a = Emit (e <> e') (f a)
  Fail <*> _ = Fail
  _ <*> Fail = Fail

deriving via Monoid.Alt Result instance Alternative Result

emptyResult :: Monoid a => Path a
emptyResult = pure mempty

newtype PathF a = PathF { runPathF :: Vty.Event -> a -> Result a }

newtype Path a = Path { runPath :: Alt PathF a }
  deriving newtype (Functor, Applicative, Alternative)

liftPathF :: PathF a -> Path a
liftPathF = Path . liftAlt

result :: a -> Path a
result = pure

switch :: (Vty.Event -> a -> Result a) -> Path a
switch fn = Path (liftAlt (PathF fn))

emitting :: a -> GameAction -> Result a
emitting a e = Emit (pure e) a

-- Coalgebra describing how components unfold into an event tree
class Responder a where respondTo :: a -> Path a

respond :: Responder a => Vty.Event -> a -> Result a
respond e a = eval (respondTo a) e a

-- Using two traversals, rewrite the Responder pointed to by the second
-- should the first succeed.
try ::
  (Is setter A_Traversal, Is query A_Fold, Responder b) =>
  Optic' query is1 a unused ->
  Optic' setter is2 a b ->
  a ->
  Path a
try getit setit a = recurse setit a <* guard (has getit a)

whenMatches :: (Is query A_Fold) => Optic' query is (Event () a) unused -> (a -> Result a) -> a -> Path a
whenMatches opt fn a = switch $ (\e _ -> if has opt (Event e a ()) then fn a else Fail)

overState :: Is k A_Fold => Optic' k is (Event () a) contents -> (a -> a) -> a -> Path a
overState opt fn = whenMatches opt (Ok . fn)

recurse :: (Is k A_Traversal, Responder b) => Optic' k is a b -> a -> Path a
recurse opt = traverseOf opt respondTo


eval :: forall a. Path a -> Vty.Event -> a -> Result a
eval (Path (Alt xs)) = _
