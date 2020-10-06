module Game.Command where

import Data.Message (Message)
import Game.Canvas (Canvas)
import Game.Info (Info)

data Command
  = Redraw Canvas
  | Update Info
  | Notify Message
