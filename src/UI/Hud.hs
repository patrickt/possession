{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module UI.Hud
  ( Hud (Hud),
    initial,
  )
where

import Data.Message qualified as Message
import Data.Position (Position, pattern (:-), HasPosition (position))
import Game.Info (Info)
import Optics
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty
import UI.Responder
import UI.Event (keypress, shiftOn)
import Data.Name
import Data.Maybe
import Data.List.Pointed qualified as PL
import qualified Data.Map.Strict as Map
import Data.List (sort)

data Hud = Hud
  { hudPosition :: Position
  , hudTargets :: PL.PointedList Position
  }
  deriving stock Generic
  deriving anyclass HasPosition

makeFieldLabels ''Hud

instance Responder Hud where
  respondTo =
    perEnemy
    <|> standardMovement
    where
      perEnemy = moveByList (Vty.KChar 'j') PL.previous <|> moveByList (Vty.KChar 'k') PL.next
      standardMovement =
        move Vty.KUp (0 :- negate 1)
        <|> move Vty.KDown (0 :- 1)
        <|> move Vty.KLeft (negate 1 :- 0)
        <|> move Vty.KRight (1 :- 0)
      move k amt = overState (keypress k) (over #position (+amt) :: Hud -> Hud)
      moveByList k fn =
        ensuring (has (keypress k))
        >>> arr (over #targets fn)
        >>> arr (\s -> s & #position .~ (s ^. #targets % PL.focus))

initial :: Info -> Hud
initial info = Hud player (fromMaybe initial (PL.find player initial))
  where
    initial = PL.fromList (sort (player : allEnemies))
    player = info ^. position
    allEnemies = info ^. #summary % to Map.keys

insertReadout :: Position -> Info -> Modeline -> Modeline
insertReadout p i m = case i ^? #summary % ix p % name of
  Just n -> m & Modeline.display (Message.youSee n)
  _ -> m
