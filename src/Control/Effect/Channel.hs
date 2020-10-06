{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Control.Effect.Channel
  ( writeB
  ) where

import Control.Effect.Reader
import Brick.BChan
import Control.Monad.IO.Class

writeB :: forall msg sig m . (MonadIO m, Has (Reader (BChan msg)) sig m) => msg -> m ()
writeB x = do
  chan <- ask @(BChan msg)
  liftIO (writeBChan chan x)
