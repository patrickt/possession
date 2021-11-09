{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module UI.Responder.Chain
  ( Chain (..),
    SomeResponder (..),
    push,
    pop,
    propagate,
    first,
    castTo,
  )
where

import Control.Monad.ST
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.STRef
import Data.Traversable
import Data.Typeable
import Optics
import UI.Render (Renderable (..))
import UI.Responder
import GHC.Exts

newtype Chain = Chain { unChain :: NonEmpty SomeResponder }

instance Renderable Chain where
  render (Chain c) stack = foldr render stack c

instance IsList Chain where
  type Item Chain = SomeResponder
  fromList = Chain . fromList
  toList = toList . unChain

-- | Add a new element.
push :: SomeResponder -> Chain -> Chain
push = coerce (NonEmpty.cons @SomeResponder)

-- | Pop an element. No-op if there is only one item on the chain.
pop :: Chain -> Chain
pop (Chain (_ :| (y : z))) = Chain (y :| z)
pop x = x

-- | Given a function on some type implementing 'Responder', walk down the chain
-- and try to apply it, using dynamic casting.
propagate :: forall a. (Renderable a, Responder a, Typeable a) => (a -> a) -> Chain -> Chain
propagate fn (Chain xs) = runST do
  -- I could perhaps write this with callCC but it sounds even harder.
  found <- newSTRef False
  new <- for xs \x -> do
    present <- readSTRef found
    if present
      then pure x
      else case x ^? castTo of
        Just v -> writeSTRef found True >> pure (SomeResponder (fn v))
        Nothing -> pure x
  pure (Chain new)

-- | Access the first element of the responder chain.
first :: Lens' Chain SomeResponder
first = lens g s
  where
    g (Chain (x :| _)) = x
    s (Chain (_ :| y)) x = Chain (x :| y)
