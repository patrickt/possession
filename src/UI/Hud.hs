{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-unclutter-valid-hole-fits #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module UI.Hud
  ( Hud (Hud),
    initial,
  )
where

import Data.Message qualified as Message
import Data.Position (Position, pattern (:-), HasPosition (position))
import Game.Info (Info, HasInfo(..))
import Optics
import UI.Widgets.Modeline (Modeline)
import UI.Widgets.Modeline qualified as Modeline
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty
import UI.Responder
import UI.Event (keypress)
import Data.Name
import Data.Maybe
import Data.List.Pointed qualified as PL
import Data.List (sort)
import Game.Action
import qualified Game.Info as Info

data Hud a = Hud
  { hudPosition :: Position
  , hudTargets :: PL.PointedList (Position, Name)
  , hudParent :: a
  }
  deriving stock (Functor, Generic)
  deriving anyclass HasPosition

makeFieldLabels ''Hud

instance HasInfo a => HasInfo (Hud a) where info = #parent % info

instance HasInfo a => Responder (Hud a) where
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
      move k amt = overState (keypress k) (over #position (+amt))
      moveByList k fn =
        ensuring (has (keypress k))
        >>> arr (over #targets fn)
        >>> andEmit (Notify . Message.youSee . nameOfTarget)
        >>> arr (\s -> s & #position .~ (s ^. #targets % PL.focus % _1 & (+ negate 1)))

nameOfTarget :: Hud a -> Name
nameOfTarget = view (#targets % PL.focus % _2)

initial :: HasInfo a => a -> Hud a
initial p = Hud (fst player) (fromMaybe start (PL.find player start)) p
  where
    start = PL.fromList (sort (player : enemies))
    player = (p ^. info % position, "yourself")
    enemies = Info.allEnemies (p ^. info)

insertReadout :: Position -> Info -> Modeline -> Modeline
insertReadout p i m = case i ^? #summary % ix p % name of
  Just n -> m & Modeline.display (Message.youSee n)
  _ -> m
