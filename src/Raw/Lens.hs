module Raw.Lens
  ( strategy
  ) where

import Raw.Types (Strategy)
import Optics
import Data.Generics.Product

strategy :: forall a. HasType Strategy a => Lens' a Strategy
strategy = typed
