{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}

module UI.Responder
  ( Responder (..),
    Response (..),
    SomeResponder (..),
    castTo,
    CanHandle (..),
    Handler (..),
    evalHandler,
    updating,
  )
where

import Control.Applicative
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Typeable
import Game.Action (GameAction)
import Game.Info (Info)
import Graphics.Vty qualified as Vty
import Optics
import UI.Input (Input)
import UI.Input qualified as Input
import UI.Render (Renderable (..))

data Handler :: Type -> Type where
  Ok :: Response comp -> Handler comp
  Try :: (Is k An_AffineFold, Is k A_Setter, CanHandle child) => Optic' k is comp child -> Handler comp
  Alt :: Handler comp -> Handler comp -> Handler comp
  Fail :: Handler comp

updating :: comp -> Handler comp
updating = Ok . Update

instance Semigroup (Handler comp) where
  (<>) = Alt

evalHandler :: Vty.Event -> comp -> Handler comp -> Response comp
evalHandler evt comp handle = case handle of
  Ok a -> a
  Try path -> case comp ^? path of
    Just found -> case evalHandler evt found (handleEvent evt found) of
      Update new -> Update (comp & path .~ new)
      Push a -> Push a
      Pop -> Pop
      Broadcast a -> Broadcast a
      Then x y -> undefined
      Nil -> Nil
      Terminate -> Terminate
    Nothing -> Nil
  Alt Fail b -> evalHandler evt comp b
  Alt a _ -> evalHandler evt comp a
  Fail -> Nil

class CanHandle a where
  handleEvent :: Vty.Event -> a -> Handler a

-- | 'Responder's take in 'Input' events and turn them
-- into 'Response's, which the UI then handles as needed.
data Response a where
  Push :: SomeHandler -> Response a
  Pop :: Response a
  Update :: a -> Response a
  Broadcast :: GameAction -> Response a
  Then :: Response a -> Response a -> Response a
  Nil :: Response a
  Terminate :: Response a

deriving stock instance Functor Response

instance Semigroup (Response a) where
  Nil <> a = a
  a <> Nil = a
  a <> b = Then a b

instance Monoid (Response a) where
  mempty = Nil


-- | Attempt to access the contents of a 'SomeResponder' as a given type, failing if inapplicable.
castTo :: forall a. (Renderable a, Responder a, Typeable a) => AffineTraversal' SomeResponder a
castTo = atraversal go (const SomeResponder)
  where
    go s@(SomeResponder r) = maybe (Left s) Right (cast r)
