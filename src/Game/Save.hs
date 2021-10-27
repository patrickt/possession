{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Game.Save
  ( save,
    load,
    write,
    read,
  )
where

import Apecs (Component (Storage))
import Apecs qualified
import Apecs.Core qualified as Apecs
import Apecs.Stores
import Apecs.Util (EntityCounter (EntityCounter), global)
import Control.Concurrent.Async
import Control.Effect.Reader (Algebra, ask)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Amount (Amount)
import Data.ByteString qualified as ByteString
import Data.Color (Color)
import Data.Experience (XP)
import Data.Glyph (Glyph)
import Data.Hitpoints (HP)
import Data.IORef
import Data.IntMap.Strict qualified as I
import Data.Name (Name)
import Data.Position (Position)
import Data.Store (Store)
import Data.Store qualified as Store
import GHC.Generics (Generic)
import Game.Behavior (Collision)
import Game.World (World (..))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import Unsafe.Coerce
import Prelude hiding (read)

type IM = I.IntMap

-- bite my shiny metal ass
unMap :: Apecs.Stores.Map a -> IORef (IM a)
unMap = unsafeCoerce

-- TODO: this is the wrong abstraction because it precludes
-- us using Component values other than Map. there needs to be
-- some code that more
data Save = Save
  { _amt :: IM Amount,
    _col :: IM Color,
    _beh :: IM Collision,
    _gly :: IM Glyph,
    _hps :: IM HP,
    _xps :: IM XP,
    _nam :: IM Name,
    _pos :: IM Position,
    _ent :: EntityCounter
  }
  deriving stock (Show, Generic)
  deriving anyclass (Store)

-- Concurrently here?
save :: MonadIO m => World -> m Save
save (World a b c d e f g h counter) =
  let r = Concurrently . readIORef . unMap
      r' = Concurrently . readIORef . unsafeCoerce @_ @(IORef EntityCounter)
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
          <*> r' counter
   in liftIO (runConcurrently it)

clear :: forall m. MonadIO m => World -> Apecs.SystemT World m ()
clear (World a b c d e f g h _counter) =
  let go :: Apecs.Stores.Map a -> Concurrently ()
      go im = Concurrently (writeIORef (unMap im) I.empty)
   in liftIO $
        runConcurrently
          ( go a
              *> go b
              *> go c
              *> go d
              *> go e
              *> go f
              *> go g
              *> go h
          )

load :: forall m sig. (MonadIO m, Algebra sig m) => Save -> Apecs.SystemT World m ()
load (Save a b c d e f g h (EntityCounter count)) =
  let go ::
        ( Apecs.ExplSet m (Storage a),
          Apecs.Has World m a
        ) =>
        IM a ->
        Apecs.SystemT World m ()
      go = void . I.traverseWithKey (Apecs.set . Apecs.Entity)
   in do
        ask >>= clear
        go a
        go b
        go c
        go d
        go e
        go f
        go g
        go h
        setReadOnly global (EntityCounter (count + 1))

write :: MonadIO m => Save -> m ()
write s = liftIO do
  home <- liftIO getHomeDirectory
  let path = home </> ".possession"
  createDirectoryIfMissing False path
  ByteString.writeFile (path </> "save") (Store.encode s)

read :: MonadIO m => m Save
read = liftIO do
  home <- getHomeDirectory
  let path = home </> ".possession"
  contents <- ByteString.readFile (path </> "save")
  Store.decodeIO contents
