{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Widgets.Modeline
  ( Modeline,
    initial,
    messages,
    update,
    render,
  )
where

import Brick qualified
import Brick.Widgets.List qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Data.Message ( Message )
import Data.Monoid.Generic
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import UI.Render ( Renderable(..), renderThe )
import UI.Resource qualified as Resource

newtype Modeline = Modeline
  { messages :: Seq Message
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroup Modeline
  deriving (Monoid) via GenericMonoid Modeline

initial :: Modeline
initial = mempty

update :: Message -> Modeline -> Modeline
update m (Modeline msgs) = Modeline (msgs |> m)

instance Renderable Modeline where
  render (Modeline msgs) stack =
    let readout =
          Brick.hCenter
          . Brick.renderList (const (renderThe @Message)) False
          -- TODO: move this viewport appropriately rather than gyrating with drop
          $
            Brick.list Resource.Readout (Seq.drop (length msgs - 3) msgs) 1
        final = Brick.vLimit 3
                . Brick.viewport Resource.Modeline Brick.Vertical
                . Brick.vLimit 3
                $ readout
     in final : stack
