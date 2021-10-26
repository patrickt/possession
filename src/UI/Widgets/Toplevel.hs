{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module UI.Widgets.Toplevel
  ( Toplevel (Toplevel),
    initial,
  )
where

import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Core ((<=>), (<+>))
import Data.Maybe
import Data.Monoid
import GHC.Generics (Generic)
import Game.Action qualified as Action
import Game.Canvas (Canvas)
import Game.Canvas qualified as Canvas
import Graphics.Vty as Vty
import Linear (V2 (..))
import Optics
import UI.Hud qualified as Hud
import UI.Input qualified as Input
import UI.MainMenu qualified as MainMenu
import UI.Render
import UI.Responder
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import UI.Widgets.Sidebar (Sidebar)

data Toplevel = Toplevel
  { toplevelCanvas :: Canvas,
    toplevelSidebar :: Sidebar,
    toplevelModeline :: Modeline
  }
  deriving stock (Generic)

makeFieldLabels ''Toplevel

initial :: Toplevel
initial =
  Toplevel Canvas.empty mempty Modeline.initial

instance Responder Toplevel where
  translate (Vty.EvKey k mods) _ = case k of
    Vty.KChar 'l' -> Input.Look
    _ -> fromMaybe Input.None (Input.fromVty k mods)
  translate _ _ = Input.None

  onSend inp inf s =
    let move (x, y) = Broadcast (Action.Move (V2 x y))
        left = move (-1, 0)
        right = move (1, 0)
        down = move (0, 1)
        up = move (0, -1)
     in case inp of
          Input.Left -> left
          Input.Right -> right
          Input.Down -> down
          Input.Up -> up
          Input.Quit -> Terminate
          Input.Menu -> Push (SomeResponder MainMenu.inGame)
          Input.Look -> Push (SomeResponder (Hud.Hud (inf ^. #position % coerced % non 0)))
          _ -> Nil

instance Renderable Toplevel where
  render (Toplevel canvas sidebar modeline) stack =
    renderOne sidebar <+> Brick.vBorder <+> (renderOne canvas <=> renderOne modeline) : stack
