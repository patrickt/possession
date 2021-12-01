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
import UI.Hud qualified as Hud

data Canvas = Canvas
  { canvasData :: Game.Canvas
  , canvasHud :: Maybe Hud.Hud
  } deriving stock Generic

makeFieldLabels ''Canvas

instance Show Canvas where show = const "Canvas"

initial :: Canvas
initial = Canvas { canvasData = Game.Canvas.empty, canvasHud = Nothing }

instance Renderable Canvas where
  draw canv =
    let allLines = scanline canv <$> [0 .. Game.Canvas.size]
        withCursor = maybe id (Brick.showCursor UI.Resource.Canvas) (canv ^? #hud % _Just % position % brickLocation)
     in pure
        . withCursor
        . Brick.viewport UI.Resource.Canvas Brick.Both
        . Brick.raw
        $ Vty.vertCat allLines

instance Responder Canvas where
  respondTo = quit
    <|> esc
    <|> look
    <|> within (#hud % _Just)
    <|> move Vty.KUp (0 :- negate 1)
    <|> move Vty.KDown (0 :- 1)
    <|> move Vty.KLeft (negate 1 :- 0)
    <|> move Vty.KRight (1 :- 0)
    where
      look = overState (keypress (Vty.KChar '*')) (set #hud (Just Hud.initial))
      esc = ensuring (has (#state % #hud %_Just)) >>> overState (keypress Vty.KEsc) (set #hud Nothing)
      quit = whenMatches (keypress (Vty.KChar 'q')) (`emitting` Terminate)
      move k amt = whenMatches (keypress k) (`emitting` Move amt)


scanline :: Canvas -> Int -> Vty.Image
scanline (Canvas canv _) idx = Vty.horizCat do
  x <- [0 .. Game.Canvas.size]
  pure . drawSprite . Game.Canvas.at canv $ x :- idx

drawSprite :: Sprite -> Vty.Image
drawSprite (Sprite (Glyph chr) color bgcolor) = Vty.char attr chr
  where
    attr = Attr.currentAttr {Attr.attrForeColor = Attr.SetTo (colorToVty color),
                            Attr.attrBackColor = Attr.SetTo (colorToVty bgcolor)}

update :: Game.Canvas -> Canvas -> Canvas
update = set #data
