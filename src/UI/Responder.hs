{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
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
    pop
  )
where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import UI.Input (Input)
import UI.Render (Renderable (..))
import Data.Coerce
import Game.Action
import Optics
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

data SomeResponder = forall x. (Renderable x, Responder x) => SomeResponder x

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

first :: Lens' Chain SomeResponder
first = lens g s
  where
    g (Chain (x :| _)) = x
    s (Chain (_ :| y)) x = Chain (x :| y)
