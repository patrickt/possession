-- | 'Command' values are sent from the game thread to the UI thread
-- to provoke UI changes.
module Game.Command (Command (..)) where

import Data.Message (Message)
import Game.Canvas (Canvas)
import Game.Info (Info)

data Command
  = Redraw Canvas
  | Update Info
  | Notify Message
