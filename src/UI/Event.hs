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
