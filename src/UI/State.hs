{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Control.Effect.Reader

data State = State
  { stateToplevel :: Toplevel.Toplevel,
    stateMenu :: Maybe MainMenu.MainMenu,
    stateBrokerage :: Brokerage,
    stateGameThread :: ThreadId
  }
  deriving (Generic)

instance Show State where show = const "State"

makeFieldLabels ''State

instance Responder State where
  respondTo = upon \a -> do
    evt <- ask
    let
      shouldPop = folding (\_ -> if has (#menu % _Nothing) a && has _Escape evt then Just () else Nothing)
      bringUpMenu = whenMatches shouldPop (pure . set #menu (Just MainMenu.inGame))
      menuOpen = try (#menu % _Just) #menu
      recur = recurse #toplevel
    invoke (menuOpen <|> recur <|> bringUpMenu) a


instance Renderable State where
  layout = Modal <$> preview (#menu % _Just % laidOut) <*> view (#toplevel % laidOut)

initial :: Brokerage -> ThreadId -> State
initial =
  State Toplevel.initial (Just MainMenu.initial)
