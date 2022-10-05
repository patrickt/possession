{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.App (app) where

import Brick qualified
import Brick.Widgets.Center qualified as Brick
import Control.Effect.Broker qualified as Broker
import Control.Monad.IO.Class (liftIO)
import Data.Foldable
import Game.Action as Action
import Optics
import UI.Render qualified as Render
import UI.Resource (EventM, Widget)
import UI.Resource qualified as UI (Resource)
import Graphics.Vty qualified as Vty
import UI.Responder qualified as Responder
import UI.State qualified as State
import UI.State qualified as UI (State)
import UI.Widgets.Modeline qualified as Modeline

draw :: UI.State -> [Widget]
draw s = pure . Brick.hCenter . Render.runDraw (s ^. #info) $ s

app :: Brick.App UI.State UIAction UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.showFirstCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appAttrMap = \_ -> Brick.attrMap Vty.defAttr mempty,
      Brick.appStartEvent = do
        s <- Brick.get
        liftIO $ Broker.enqueueGameAction (s ^. #brokerage) Action.Start
    }

handleEvent :: Brick.BrickEvent UI.Resource UIAction -> EventM UI.State ()
handleEvent e = do
  s <- Brick.get
  case e of
    Brick.VtyEvent vty -> do
      case Responder.runResponder vty s of
        Nothing -> pure ()
        Just (actions, new) -> do
          forM_ actions $ \act -> do
            liftIO . Broker.enqueueGameAction (s ^. #brokerage) $ act
          Brick.put new
    Brick.AppEvent evt -> do
      let next fn = Brick.put (fn s)
      case evt of
        Start -> pure ()
        Terminate -> Brick.halt
        Redraw canv -> next (set (#toplevel % #canvas % #data) canv)
        Update info -> next (State.updateState info)
        Notify msg -> next (over (#toplevel % #modeline) (Modeline.display msg))
    _ -> pure ()
