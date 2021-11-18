{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module UI.SimpleResponder
  ( Path (..)
  , descend
  , run
  , Responder (..)
  , module Control.Applicative
  ) where

import Game.Action
import Optics
import Data.Monoid hiding (Ap (..))
import Control.Applicative
import Data.Maybe
import Data.Kind (Type)
import qualified Control.Alternative.Free as Free
import Data.Functor.Compose
import qualified Graphics.Vty as Vty
import Data.Coerce (coerce)

data PathF :: Type -> Type where
  When :: (Vty.Event -> a) -> PathF a
  Within :: Is k A_Traversal => Optic' k is a s -> (Vty.Event -> s -> s) -> PathF a -> PathF a
  Emit :: GameAction -> PathF a -> PathF a

newtype Path a = Path { getPath :: Free.Alt (Compose First PathF) a }
  deriving newtype (Functor, Applicative, Alternative)
  deriving (Semigroup, Monoid) via Alt Path a

fromPathF :: PathF a -> Path a
fromPathF = Path . Free.liftAlt . coerce . Just

descend :: (Responder s, Is k A_Traversal) => Optic' k is a s -> a -> Path a
descend opt = fromPathF . Within opt run . When . const

class Responder a where respondTo :: a -> Path a

instance Responder a => Responder (Maybe a) where
  respondTo = maybe mempty (fmap Just . respondTo)

runPath :: forall a . Vty.Event -> Compose First PathF a -> Maybe a
runPath e (Compose (First x)) = x >>= go
  where
    go :: PathF a -> Maybe a
    go = \case
      When fn -> Just . fn $ e
      Within opt fn p -> go p >>= failover opt (fn e)
      Emit _ v -> go v

run :: Responder a => Vty.Event -> a -> a
run e a = fromMaybe a . Free.runAlt (runPath e) . getPath . respondTo $ a
