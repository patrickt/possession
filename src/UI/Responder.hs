{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UI.Responder
  ( Responder (..),
    SomeResponder (..),
    Response (..),
  )
where

import Data.Typeable
import Game.Action
import Graphics.Vty qualified as Vty
import UI.Input (Input)
import UI.Input qualified as Input
import UI.Render (Renderable (..))
import Data.Maybe (fromMaybe)

data Response a
  = Push SomeResponder
  | Update a
  | Broadcast (Action 'Game)
  | Nil
  | Pop
  | Terminate
  deriving stock (Functor)

class Responder a where
  translate :: Vty.Event -> a -> Input
  translate (Vty.EvKey k mods) _ = fromMaybe Input.None (Input.fromVty k mods)
  translate _ _ = Input.None

  onSend :: Input -> a -> Response a

data SomeResponder = forall x. (Renderable x, Responder x, Typeable x) => SomeResponder x

instance Renderable SomeResponder where
  render (SomeResponder x) = render x
  renderMany (SomeResponder x) = renderMany x

instance Responder SomeResponder where
  translate e (SomeResponder r) = translate e r
  onSend i (SomeResponder r) = fmap SomeResponder (onSend i r)
