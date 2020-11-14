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
    Response (..),
  )
where

import UI.Input (Input)
import UI.Render (Renderable (..))
import Game.Action
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
