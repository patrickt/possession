module Game.Command where

import Game.Canvas (Canvas)
import Game.Info (Info)

data Command
  = Redraw Canvas
  | Update Info
