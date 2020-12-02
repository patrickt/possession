{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Coerce (coerce)
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

instance IsList Chain where
  type Item Chain = SomeResponder
  fromList = Chain . fromList
  toList = toList . unChain

push :: SomeResponder -> Chain -> Chain
push = coerce (NonEmpty.cons @SomeResponder)

pop :: Chain -> Chain
pop (Chain (_ :| (y : z))) = Chain (y :| z)
pop x = x

propagate :: forall a. (Renderable a, Responder a, Typeable a) => (a -> a) -> Chain -> Chain
propagate fn (Chain xs) = runST $ do
  found <- newSTRef False
  new <- for xs \x -> do
    present <- readSTRef found
    if present
      then pure x
      else case x ^? castTo of
        Just v -> writeSTRef found True >> pure (SomeResponder (fn v))
        Nothing -> pure x
  pure (Chain new)

castTo :: forall a. (Renderable a, Responder a, Typeable a) => AffineTraversal' SomeResponder a
castTo = atraversal go (const SomeResponder)
  where
    go s@(SomeResponder r) = maybe (Left s) Right (cast r)

first :: Lens' Chain SomeResponder
first = lens g s
  where
    g (Chain (x :| _)) = x
    s (Chain (_ :| y)) x = Chain (x :| y)
