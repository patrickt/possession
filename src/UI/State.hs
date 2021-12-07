{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funclutter-valid-hole-fits #-}

-- | Top-level state container used by the Brick app.
module UI.State
  ( State (State),
    initial,
  updateState)
where

import Control.Concurrent (ThreadId)
import Control.Effect.Broker (Brokerage)
import GHC.Generics (Generic)
import Optics
import UI.Responder
import UI.Render
import qualified UI.Widgets.MainMenu as MainMenu
import qualified UI.Widgets.Toplevel as Toplevel
import UI.Event
import Game.Info qualified as Game (Info)
import Game.Info

data State = State
  { stateToplevel :: Toplevel.Toplevel State,
    stateMenu :: Maybe MainMenu.MainMenu,
    stateInfo :: Game.Info,
    stateBrokerage :: Brokerage,
    stateGameThread :: ThreadId
  }
  deriving (Generic)

makeFieldLabels ''State

instance HasInfo State where info = #info

instance Responder State where
  respondTo =
    let
      shouldPop = ensuring (\evt -> has (#state % #menu % _Nothing) evt && has (#vty % _Escape) evt)
      bringUpMenu = shouldPop >>> arr (set #menu (Just MainMenu.inGame))
      menuOpen = try (#menu % _Just) #menu
      recur = recurse #toplevel
    in menuOpen <|> recur <|> bringUpMenu


instance Renderable State where
  -- todo write helper for modal
  layout a = case a ^. #menu of
    Nothing -> a ^. #toplevel % laidOut
    Just x -> x ^. laidOut

initial :: Brokerage -> ThreadId -> State
initial x y = z
  where
    z = State (Toplevel.initial z) (Just MainMenu.initial) mempty x y

updateState :: Info -> State -> State
updateState i =  impose . set info i

impose :: State -> State
impose it = it { stateToplevel = fmap (const it) (stateToplevel it) }
