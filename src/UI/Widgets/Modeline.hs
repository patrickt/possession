{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.Modeline
  ( Modeline,
    initial,
    update,
    lastMessage,
  )
where

import Brick qualified
import Brick.Widgets.List qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Data.Message ( Message )
import Data.Monoid.Generic
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import UI.Render ( Renderable(..), runDraw )
import UI.Resource qualified as Resource
import Optics
import UI.Responder
import qualified Data.Message as Message
import Game.Info (Info)
import Control.Effect.Reader

newtype Modeline = Modeline
  { messages :: Seq Message
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Modeline
  deriving (Monoid) via GenericMonoid Modeline

initial :: Modeline
initial = mempty

update :: Message -> Modeline -> Modeline
update m (Modeline (firsts :> last'))
  | Message.mergeable m last' = Modeline (firsts :> mappend last' m)
  | otherwise = Modeline (firsts :> last' :> m)
update m _ = Modeline (pure m)

lastMessage :: AffineTraversal' Modeline Message
lastMessage = #messages % _last

instance Renderable Modeline where
  draw (Modeline msgs) = do
    i <- ask @Info
    let readout =
          Brick.hCenter
          . Brick.renderList (const (runDraw i)) False
          -- TODO: move this viewport appropriately rather than gyrating with drop
          $
            Brick.list Resource.Readout (Seq.drop (length msgs - 3) msgs) 1
     in pure
        . Brick.vLimit 3
        . Brick.viewport Resource.Modeline Brick.Vertical
        . Brick.vLimit 3
        $ readout

instance Responder Modeline where
  respondTo = empty
