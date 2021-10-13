{-# LANGUAGE ImportQualifiedPost #-}

module UI.Responder
  ( Responder (..),
    Response (..),
    SomeResponder (..),
    castTo,
  )
where

import Data.Maybe (fromMaybe)
import Data.Typeable
import Game.Action (GameAction)
import Graphics.Vty qualified as Vty
import Optics
import UI.Input (Input)
import UI.Input qualified as Input
import UI.Render (Renderable (..))
import Game.Info (Info)

-- | 'Responder's take in 'Input' events and turn them
-- into 'Response's, which the UI then handles as needed.
data Response a
  = -- | Push onto the chain
    Push SomeResponder
  | -- | Remove the topmost element
    Pop
  | -- | Replace the topmost element
    Update a
  | -- | Send an action back to the ECS
    Broadcast GameAction
  | -- | Chain two actions
    Then (Response a) (Response a)
  | -- | Do nothing
    Nil
  | -- | Shut down the whole app
    Terminate
  deriving stock (Functor)

instance Semigroup (Response a) where
  (<>) = Then

instance Monoid (Response a) where
  mempty = Nil

-- | Defines UI elements that are able to handle inputs.
class Responder a where
  -- | Turn a 'Vty.Event' into an 'Input'. By default this calls out to 'Input.fromVty',
  -- but you can override it if necessary.
  translate :: Vty.Event -> a -> Input
  translate (Vty.EvKey k mods) _ = fromMaybe Input.None (Input.fromVty k mods)
  translate _ _ = Input.None

  -- | Handle an 'Input' and turn it into a 'Response'.
  -- I think this needs to have the most recent 'UI.State' passed in, too.
  onSend :: Input -> Info -> a -> Response a

-- | Existential constructor that enables the heterogenous nature of the responder chain.
data SomeResponder = forall x. (Renderable x, Responder x, Typeable x) => SomeResponder x

instance Renderable SomeResponder where
  render (SomeResponder x) = render x

instance Responder SomeResponder where
  translate e (SomeResponder r) = translate e r
  onSend inp inf (SomeResponder r) = fmap SomeResponder (onSend inp inf r)

-- | Attempt to access the contents of a 'SomeResponder' as a given type, failing if inapplicable.
castTo :: forall a. (Renderable a, Responder a, Typeable a) => AffineTraversal' SomeResponder a
castTo = atraversal go (const SomeResponder)
  where
    go s@(SomeResponder r) = maybe (Left s) Right (cast r)
