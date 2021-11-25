{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Canvas
  ( Canvas (Canvas),
    initial,
    update
  )
where

import Brick qualified
import Data.Position
import Game.Canvas qualified
import Game.Canvas qualified as Game (Canvas)
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as Attr
import Data.Glyph
import Optics
import UI.Event
import UI.Render
import Game.Sprite
import UI.Resource qualified
import GHC.Generics (Generic)
import UI.Responder
import Game.Action
import Debug.Trace
import Control.Applicative

data Canvas = Canvas
  { canvasData :: Game.Canvas,
    canvasShowsCursor :: Bool
  } deriving stock Generic

makeFieldLabels ''Canvas

instance Show Canvas where show = const "Canvas"

initial :: Canvas
initial = Canvas { canvasData = Game.Canvas.empty, canvasShowsCursor = False }

instance Renderable Canvas where
  draw canv =
    let allLines = scanline canv <$> [0 .. Game.Canvas.size]
     in Brick.viewport UI.Resource.Canvas Brick.Both
        . Brick.raw
        $ Vty.vertCat allLines

instance Responder Canvas where
  respondTo a = quit
    <|> move Vty.KUp (0 :- negate 1)
    <|> move Vty.KDown (0 :- 1)
    <|> move Vty.KLeft (negate 1 :- 0)
    <|> move Vty.KRight (1 :- 0)
    where
      quit = whenMatches (keypress (Vty.KChar 'q')) (`emitting` Terminate) a
      move k amt = whenMatches (keypress k) (`emitting` Move amt) a


scanline :: Canvas -> Int -> Vty.Image
scanline (Canvas canv _) idx = Vty.horizCat do
  x <- [0 .. Game.Canvas.size]
  pure . drawSprite . Game.Canvas.at canv $ x :- idx

drawSprite :: Sprite -> Vty.Image
drawSprite (Sprite (Glyph chr) color) = Vty.char attr chr
  where
    attr = Attr.currentAttr {Attr.attrForeColor = Attr.SetTo (colorToVty color)}

update :: Game.Canvas -> Canvas -> Canvas
update = set #data
