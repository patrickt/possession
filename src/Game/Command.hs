module Game.Command where

import Game.Canvas (Canvas)

data Command
  = Redraw Canvas
