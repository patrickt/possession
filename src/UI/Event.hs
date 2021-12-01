{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Event where

import Optics
import Graphics.Vty qualified as Vty
import Data.Generics.Sum

-- Encapsulates a Vty event, a component's state, and some extra, propagatable info
data Event state = Event {eventVty :: Vty.Event, eventState :: state }
  deriving stock Functor

makeFieldLabels ''Event

keypress k = #vty % _Ctor @"EvKey" % _1 % only k

evkey :: Prism' Vty.Event (Vty.Key, [Vty.Modifier])
evkey = _Ctor @"EvKey"

_Escape :: Optic An_AffineTraversal NoIx Vty.Event Vty.Event () ()
_Escape = evkey % _1 % only Vty.KEsc

shiftOn :: Event a -> Bool
shiftOn e = Vty.MShift `elem` (e ^.. #vty % evkey % _2 % folded)
