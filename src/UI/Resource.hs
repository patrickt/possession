module UI.Resource
  ( Resource (..),
    Widget,
    Event,
    EventM,
  )
where

import qualified Brick
import Game.Action (UIAction)

-- | Names visible widgets in the UI.
data Resource
  = Modeline
  | Canvas
  | MainMenu
  | Look
  | Readout
  | Unspecified
  deriving (Eq, Show, Ord)

type Widget = Brick.Widget Resource
type Event = Brick.BrickEvent Resource UIAction
type EventM = Brick.EventM Resource
