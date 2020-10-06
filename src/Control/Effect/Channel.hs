{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Effect.Channel
  ( writeB,
  )
where

import Brick.BChan
import Control.Effect.Reader
import Control.Monad.IO.Class

writeB :: forall msg sig m. (MonadIO m, Has (Reader (BChan msg)) sig m) => msg -> m ()
writeB x = do
  chan <- ask @(BChan msg)
  liftIO (writeBChan chan x)
