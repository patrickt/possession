{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module UI.SimpleResponder
  ( Path (..),
    Responder (..),
    Event,
    accept,
    respond,
    try,
    recurse,
    eval,
  )
where

import Control.Applicative
import Data.Kind (Type)
import Graphics.Vty qualified as Vty
import Optics
import Game.Action (GameAction)
import UI.Event

-- A binary tree of actions that, given some state type and a Vty event,
-- can evaluate to success/failure. If this were of type T -> T -> T,
-- it could implement Profunctor.
data Path :: Type -> Type where
  Fail :: Path a
  Try :: Path a -> Path a -> Path a
  -- First parameter represents a query operation that, if it succeeds,
  -- is passed to the second operation, which can in turn succeed/fail.
  -- The forall'ed `x` variable here prevents functoriality/applicativeness
  When :: (Event () a -> Maybe x) -> (Event x a -> Maybe a) -> Path a
  -- Send an action back to the ECS
  Emit :: GameAction -> Path a -> Path a

-- Path is Pointed, but not a functor or applicative
accept :: a -> Path a
accept = When Just . const . Just

instance Semigroup (Path a) where
  Fail <> a = a
  a <> Fail = a
  a <> b = Try a b

instance Monoid (Path a) where mempty = Fail

-- Coalgebra describing how components unfold into an event tree
class Responder a where respondTo :: a -> Path a

respond :: Responder a => Vty.Event -> a -> Maybe a
respond e a = eval (respondTo a) e a

-- Using two traversals, rewrite the Responder pointed to by the second
-- should the first succeed.
try ::
  (Is setter A_Traversal, Is query An_AffineFold, Responder b) =>
  Optic' query is1 (Event () a) unused ->
  Optic' setter is2 a b ->
  Path a
try getit setit = When (preview getit) (\(Event vty state _) -> traverseOf setit (respond vty) state)

recurse :: (Is k A_Traversal, Responder b) => Optic' k is a b -> Path a
recurse = try (to Just)

actions :: Path a -> Vty.Event -> a -> [GameAction]
actions p e s = go [] p
  where
    go xs = \case
      Fail -> []
      Try l r -> go xs l <|> go xs r
      When check _ -> maybe [] (const xs) (check (Event e s ()))
      Emit evt next -> go (evt:xs) next

eval :: forall a. Path a -> Vty.Event -> a -> Maybe a
eval p e s = go p
  where
    go :: Path a -> Maybe a
    go = \case
      Fail -> Nothing
      Try a b -> go a <|> go b
      Emit _ a -> go a
      When predicate action -> predicate (Event e s ()) >>= action . Event e s
