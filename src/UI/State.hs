{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}

-- | Top-level state container used by the Brick app.
module UI.State
  ( State (State),
    initial,
  )
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

data State = State
  { stateToplevel :: Toplevel.Toplevel,
    stateMenu :: Maybe MainMenu.MainMenu,
    stateInfo :: Game.Info,
    stateBrokerage :: Brokerage,
    stateGameThread :: ThreadId
  }
  deriving (Generic)

instance Show State where show = const "State"

makeFieldLabels ''State

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
initial =
  State Toplevel.initial (Just MainMenu.initial) mempty
