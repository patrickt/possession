module Game.Command where

import Game.Canvas (Canvas)
import Game.Info (Info)
import Data.Message (Message)

data Command
  = Redraw Canvas
  | Update Info
  | Notify Message
