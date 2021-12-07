{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Game.Save
  ( save,
    load,
    write,
    read,
  )
where

import Apecs qualified
import Apecs.Exts ()
import Apecs.Util (global)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as ByteString
import Game.Entity.Player (Player)
import Data.Store (Store)
import Data.Store qualified as Store
import GHC.Generics (Generic)
import Game.World qualified as World
import Game.World (WorldT)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import Prelude hiding (read)
import Data.Foldable (traverse_)
import Control.Exception (Exception (displayException), throw)
import Game.Entity.Enemy (Enemy)
import Game.Entity.Terrain (Terrain)
import Data.Generics.Sum
import Control.Effect.State
import Raws (Raws)
import Optics
import Data.Maybe (fromJust)

data Save = Save
  { version :: Int
  , globalPlayer :: Player
  , enemies :: [Enemy]
  , terrain :: [Terrain]
  , raws :: Raws
  } deriving stock (Generic)
    deriving anyclass Store

newtype PersistenceError = BadVersion Int
  deriving (Eq, Show)

instance Exception PersistenceError where
  displayException = \case
    BadVersion n -> "bad save version: expected " <> show World.VERSION <> ", got " <> show n

_Enemy :: Prism' Enemy _
_Enemy = _Ctor @"Enemy"

(^?!) :: Is k An_AffineFold => s -> Optic' k is s a -> a
a ^?! b = fromJust (a ^? b)

save :: (Has (State Raws) sig m, MonadIO m) => Apecs.SystemT World.World m Save
save = Save World.VERSION
  <$> Apecs.get Apecs.global
  <*> Apecs.cfold (\acc it -> review _Enemy it : acc) []
  <*> Apecs.cfold (flip (:)) []
  <*> get


load :: (Has (State Raws) sig m, MonadIO m) => Save -> WorldT m ()
load (Save ver gp enem terr r) = do
  when (ver /= World.VERSION) (throw (BadVersion ver))
  Apecs.set global gp
  traverse_ (Apecs.newEntity_ . (^?! _Enemy)) enem
  traverse_ Apecs.newEntity_ terr
  put r

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
