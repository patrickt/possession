{-# LANGUAGE GADTs #-}

module UI.SimpleResponder
  ( Path (..)
  , propagateChanges
  , findActions
  , recurse
  , runResponder
  , Responder (..)
  ) where

import Game.Action
import Optics

class Responder a where respondTo :: a -> Path a

data Path a where
  Ok :: a -> Path a
  Fail :: Path a
  Alt :: Path a -> Path a -> Path a
  Try :: Responder b => (a -> Maybe b) -> (b -> a -> a) -> Path a
  Emit :: GameAction -> Path a -> Path a

instance Semigroup (Path a) where
  Fail <> a = a
  a <> Fail = a
  a <> b = Alt a b

instance Monoid (Path a) where
  mempty = Fail

instance Responder a => Responder (Maybe a) where
  respondTo = maybe mempty (go . respondTo)
    where
      go = \case
        Ok a -> Ok (Just a)
        Fail -> Fail
        Alt l r -> Alt (go l) (go r)
        Emit act q -> Emit act (go q)
        Try getter setter -> Try (>>= getter) (fmap . setter)

propagateChanges :: Responder a => Path a -> a -> a
propagateChanges p = case p of
  Ok x -> const x
  Fail -> id
  Try fn setter -> \x ->
    case fn x of
      Nothing -> x
      Just val -> setter (runResponder val) x
  Alt Fail p' -> propagateChanges p'
  Alt p' _ -> propagateChanges p'
  Emit _ p' -> propagateChanges p'

runResponder :: Responder a => a -> a
runResponder x = propagateChanges (respondTo x) x

recurse :: (k `Is` An_AffineFold, k `Is` A_Setter, Responder child) => Optic' k is parent child -> Path parent
recurse opt = Try (preview opt) (over opt . propagateChanges . respondTo)

findActions :: Path a -> a -> [GameAction]
findActions p x = go p []
  where
    go (Emit act p') = go p' . (act:)
    go (Ok _) = id
    go Fail = id
    go (Alt Fail p') = go p'
    go (Alt p' _) = go p'
    go (Try fn _) =
      case fn x of
        Nothing -> mempty
        Just _ -> id
