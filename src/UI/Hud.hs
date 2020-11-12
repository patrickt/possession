{-# LANGUAGE ImportQualifiedPost #-}

module UI.Hud where

import Data.Position (Position)
import qualified Brick
import UI.Resource qualified as Resource
import UI.Render
import qualified Brick.Widgets.Border as Brick

newtype Hud = Hud Position

instance Renderable Hud where
  render = Brick.txt "error: needs renderMany"
  renderMany =
        [ Brick.showCursor Resource.Look (pos ^. to curPos % to Brick.Location) . Attributes.withStandard . Brick.border . Brick.vBox $
            [ Brick.hBox
                [ Brick.hLimit 25 . Brick.border . render . view #sidebar $ s,
                  Brick.border . Brick.padBottom Brick.Max . Brick.reportExtent Resource.Canvas . render . view #canvas $ s
                ],
              Brick.hBorder,
              Brick.vLimit 3 . render . view #modeline $ s
            ]
        ]
