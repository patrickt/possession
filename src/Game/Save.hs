{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Save (save, load) where

import Apecs (Component (Storage))
import Apecs qualified
import Apecs.Core qualified as Apecs
import Apecs.Stores qualified
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Amount (Amount)
import Data.Color (Color)
import Data.Experience (XP)
import Data.Glyph (Glyph)
import Data.Hitpoints (HP)
import Data.IORef
import Data.IntMap.Strict qualified as I
import Data.Name (Name)
import Data.Position (Position)
import Data.Store (Store)
import GHC.Generics (Generic)
import Game.Behavior (Collision)
import Game.World (World (..))
import Unsafe.Coerce

type IM = I.IntMap

-- bite my shiny metal ass
unMap :: Apecs.Stores.Map a -> IORef (IM a)
unMap = unsafeCoerce

data Save = Save
  { _amt :: IM Amount,
    _col :: IM Color,
    _beh :: IM Collision,
    _gly :: IM Glyph,
    _hps :: IM HP,
    _xps :: IM XP,
    _nam :: IM Name,
    _pos :: IM Position
  }
  deriving stock (Show, Generic)
  deriving anyclass (Store)

-- Concurrently here?
save :: World -> IO Save
save (World a b c d e f g h _counter) =
  let r = Concurrently . readIORef . unMap
      it =
        Save
          <$> r a
          <*> r b
          <*> r c
          <*> r d
          <*> r e
          <*> r f
          <*> r g
          <*> r h
   in runConcurrently it

load :: forall m. MonadIO m => Save -> Apecs.SystemT World m ()
load (Save a b c d e f g h) =
  let go ::
        ( Apecs.ExplSet m (Storage a),
          Apecs.Has World m a
        ) =>
        IM a ->
        Apecs.SystemT World m ()
      go = void . I.traverseWithKey (Apecs.set . Apecs.Entity)
   in do
        go a
        go b
        go c
        go d
        go e
        go f
        go g
        go h
