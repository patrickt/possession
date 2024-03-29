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

import Apecs.Exts qualified as Apecs
import Apecs.Util (global)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as ByteString
import Game.Entity.Enemy (_Enemy)
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
import Control.Effect.State
import Raws (Raws)
import Optics
import Control.Effect.Trace

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

save :: (Has (State Raws) sig m, MonadIO m, Has Trace sig m) => Apecs.SystemT World.World m Save
save = do
  s <- Save World.VERSION
       <$> Apecs.get Apecs.global
       <*> Apecs.cfold (\acc it -> review _Enemy it : acc) []
       <*> Apecs.cfold (flip (:)) []
       <*> get

  trace ("count of saves: " <> show (length (terrain s)))
  trace ("count of enemies: " <> show (length (terrain s)))
  pure s


load :: (Has Trace sig m, Has (State Raws) sig m, MonadIO m) => Save -> WorldT m ()
load (Save ver gp enem terr r) = do
  when (ver /= World.VERSION) (throw (BadVersion ver))
  Apecs.set global gp

  traverse_ (Apecs.newEntity_ . preview _Enemy) enem
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
