{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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

-- Encapsulates a Vty event, a component's state, and some extra, propagatable info
data Event contents state = Event {eventVty :: Vty.Event, eventState :: state, eventContents :: contents}
  deriving stock Functor

makeFieldLabels ''Event

data Path :: Type -> Type where
  Fail :: Path a
  Try :: Path a -> Path a -> Path a
  When :: (Event () a -> Maybe x) -> (Event x a -> Maybe a) -> Path a

accept :: a -> Path a
accept a = When Just . const $ Just a

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
  (Is k1 A_Traversal, Is k2 An_AffineFold, Responder b) =>
  Optic' k2 is1 (Event () a) unused ->
  Optic' k1 is2 a b ->
  Path a
try getit setit = When (preview getit) (\(Event vty state _) -> traverseOf setit (respond vty) state)

recurse :: (Is k A_Traversal, Responder b) => Optic' k is a b -> Path a
recurse = try (to Just)

eval :: forall a. Path a -> Vty.Event -> a -> Maybe a
eval p e s = go p
  where
    go :: Path a -> Maybe a
    go = \case
      Fail -> Nothing
      Try a b -> go a <|> go b
      When predicate action -> predicate (Event e s ()) >>= action . Event e s
