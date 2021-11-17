{-# LANGUAGE GADTs #-}

module UI.SimpleResponder
  ( Path (..)
  , propagateChanges
  , findActions
  , Responder (..)
  ) where

import Game.Action
import Optics

data Path a where
  Ok :: Path a
  Fail :: Path a
  Alt :: Path a -> Path a -> Path a
  Try :: Responder b => (a -> Maybe b) -> (a -> b -> a) -> Path a
  Emit :: GameAction -> Path a -> Path a

instance Semigroup (Path a) where
  Fail <> a = a
  a <> Fail = a
  a <> b = Alt a b

instance Monoid (Path a) where
  mempty = Fail

class Responder a where respondTo :: a -> Path a

propagateChanges :: Responder a => Path a -> a -> a
propagateChanges p = case p of
  Ok -> id
  Fail -> id
  Try fn setter -> \x ->
    case fn x of
      Nothing -> x
      Just val -> setter x (propagateChanges (respondTo val) val)
  Alt Fail p' -> propagateChanges p'
  Alt p' _ -> propagateChanges p'
  Emit _ p' -> propagateChanges p'

recurse :: (k `Is` An_AffineFold, k `Is` A_Setter, Responder child) => Optic' k is parent child -> Path parent
recurse opt = Try (preview opt) (\x y -> x & opt %~ propagateChanges (respondTo y))

findActions :: Path a -> a -> [GameAction]
findActions p x = go p []
  where
    go (Emit act p') = go p' . (act:)
    go Ok = id
    go Fail = id
    go (Alt Fail p') = go p'
    go (Alt p' _) = go p'
    go (Try fn _) =
      case fn x of
        Nothing -> mempty
        Just _ -> id
