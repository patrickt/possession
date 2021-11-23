{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.App (app) where

import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Control.Effect.Broker qualified as Broker
import Control.Monad.IO.Class (liftIO)
import Game.Action as Action
import Optics
import UI.Render qualified as Render
import UI.Resource (Event, EventM, Widget)
import UI.Resource qualified as UI (Resource)
import UI.State qualified as UI (State)
import UI.Responder qualified as Responder
import Data.Foldable
import qualified UI.Widgets.Modeline as Modeline
import Data.Maybe
import Control.Monad

draw :: UI.State -> [Widget]
draw = pure . Brick.hCenter . Render.draw

app :: Brick.App UI.State UIAction UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.showFirstCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appAttrMap = \_ -> Brick.attrMap mempty mempty,
      Brick.appStartEvent = \s -> do
        liftIO $ Broker.enqueueGameAction (s ^. #brokerage) Action.Start
        pure s
    }

handleEvent :: UI.State -> Event -> EventM UI.State
handleEvent s e = case e of
  Brick.VtyEvent vty -> do
    let newState = fromMaybe s (Responder.respond vty s)
    let actions = Responder.actions (Responder.respondTo @UI.State) vty s
    forM_ actions $ \act -> do
      liftIO . Broker.enqueueGameAction (s ^. #brokerage) $ act
      liftIO $ print act
    when (null actions) (liftIO (print "no actions"))
    Brick.continue newState
  Brick.AppEvent e -> do
    let next fn = Brick.continue (fn s)
    case e of
      Start -> Brick.continue s
      Terminate -> Brick.halt s
      Redraw canv -> next (set (#toplevel % #canvas % #data) canv)
      Update info -> next (set (#toplevel % #sidebar % #info) info)
      Notify msg -> next (over (#toplevel % #modeline) (Modeline.update msg))
  _ -> Brick.continue s
