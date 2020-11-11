{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Responder
  ( Responder (..),
    SomeResponder (..),
    Chain (..),
    Response (..),
    first,
    push,
    propagate,
    castTo,
    pop
  )
where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import UI.Input (Input)
import UI.Render (Renderable (..))
import Data.Traversable
import Data.STRef
import Control.Monad.ST
import Game.Action
import Optics
import Data.Typeable
import qualified Graphics.Vty as Vty

data Response a
  = Push SomeResponder
  | Update a
  | Broadcast (Action 'Game)
  | Nil
  | Pop
  | Terminate
    deriving stock Functor

class Responder a where
  translate :: Vty.Event -> a -> Input

  onSend :: Input -> a -> Response a

data SomeResponder = forall x. (Renderable x, Responder x, Typeable x) => SomeResponder x

instance Renderable SomeResponder where
  render (SomeResponder x) = render x
  renderMany (SomeResponder x) = renderMany x

instance Responder SomeResponder where
  translate e (SomeResponder r) = translate e r
  onSend i (SomeResponder r) = fmap SomeResponder (onSend i r)

newtype Chain = Chain (NonEmpty SomeResponder)

push :: SomeResponder -> Chain -> Chain
push r (Chain x) = Chain (NonEmpty.cons r x)

pop :: Chain -> Chain
pop (Chain (_ :| (y:z))) = Chain (y :| z)
pop x = x

propagate :: forall a . (Renderable a, Responder a, Typeable a) => (a -> a) -> Chain -> Chain
propagate fn (Chain xs) = runST $ do
  found <- newSTRef False
  new <- for xs $ \x -> do
    present <- readSTRef found
    if present
      then pure x
      else case x ^? castTo of
        Just v -> writeSTRef found True >> pure (SomeResponder (fn v))
        Nothing -> pure x
  pure (Chain new)

castTo :: forall a . (Renderable a, Responder a, Typeable a) => AffineTraversal' SomeResponder a
castTo = casting % _Just
  where
    casting :: Lens' SomeResponder (Maybe a)
    casting = lens (\(SomeResponder s) -> cast s) go
    go :: SomeResponder -> Maybe a -> SomeResponder
    go x Nothing = x
    go _ (Just v) = SomeResponder v

first :: Lens' Chain SomeResponder
first = lens g s
  where
    g (Chain (x :| _)) = x
    s (Chain (_ :| y)) x = Chain (x :| y)
