{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Query
  ( query,
  ) where

import Game.Entity.Enemy (Enemy (..))
import qualified Apecs
import Game.World (World)
import Control.Monad.IO.Class

class Queryable a where
  query :: MonadIO m => Apecs.Entity -> Apecs.SystemT World m a

instance Queryable Enemy where
  query p = do
    (enemyTag, enemyName, enemyColor, enemyGlyph, enemyId) <- Apecs.get p
    (enemyPosition, enemyHearing, enemyCollision, enemyStrategy) <- Apecs.get p
    (enemyHP, enemyGold, enemyXP) <- Apecs.get p
    pure Enemy{..}
