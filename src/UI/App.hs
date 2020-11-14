{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified
import Control.Concurrent (killThread)
import Control.Effect.Broker qualified as Broker
import Control.Monad.IO.Class
import Data.Generics.Product
import Game.Action (Action, Dest (..))
import Game.Action qualified as Action
import Graphics.Vty qualified as Vty
import Optics
import UI.InGame (InGame)
import UI.Render
import UI.Resource qualified as UI (Resource)
import UI.Responder.Chain (castTo)
import UI.Responder.Chain qualified as Responder
import UI.Responder qualified as Responder
import UI.State qualified as State
import UI.State qualified as UI (State)
import UI.Widgets.Modeline qualified as Modeline

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = s ^. #responders % Responder.first % to renderMany

-- let curPos p = view components (p + offset)
--     offset = Position.make 21 3 -- TODO: figure out how to query for the offset information of the sidebar
--  in case s ^. State.mode of


event :: UI.State -> Brick.BrickEvent UI.Resource (Action 'UI) -> Brick.EventM UI.Resource (Brick.Next UI.State)
event s evt = case evt of
  Brick.VtyEvent vty -> do
    let first = s ^. #responders % Responder.first
    let inp = Responder.translate vty first
    let act = Responder.onSend inp first

    case act of
      Responder.Nil ->
        Brick.continue s
      Responder.Push r -> do
        liftIO
          . Broker.runBroker (error "no queue") (s ^. #gamePort)
          . Broker.pushAction
          $ Action.NoOp

        Brick.continue (s & #responders %~ Responder.push r)
      Responder.Pop -> do
        Brick.continue (s & #responders %~ Responder.pop)
      Responder.Update a ->
        Brick.continue (s & #responders % Responder.first .~ a)
      Responder.Broadcast go -> do
        liftIO
          . Broker.runBroker (error "no queue") (s ^. #gamePort)
          . Broker.pushAction
          $ go
        Brick.continue s
      Responder.Terminate -> do
        liftIO (killThread (s ^. #gameThread))
        Brick.halt s

  Brick.AppEvent cmd -> Brick.continue $ case cmd of
    Action.NoOp -> s
    Action.Redraw canv -> s & #responders %~ Responder.propagate @InGame (#canvas .~ canv)
    Action.Update info -> s & #responders %~ Responder.propagate @InGame (#sidebar % typed .~ info)
    Action.Notify msg -> do
      let lastMessage = State.firstResponder % castTo @InGame % #modeline % #messages % _last
      let previous = s ^? lastMessage % #contents
      let shouldCoalesce = previous == Just (msg ^. #contents)
      case (previous, shouldCoalesce) of
        (Just _, True) -> s & lastMessage % #times %~ (+ 1)
        _ -> s & #responders %~ Responder.propagate @InGame (#modeline %~ Modeline.update msg)
    _ -> s
  _ -> Brick.continue s

app :: Brick.App UI.State (Action 'UI) UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.showFirstCursor,
      Brick.appHandleEvent = event,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = \s -> do
        liftIO
          . Broker.runBroker (error "no queue") (s ^. #gamePort)
          . Broker.pushAction
          $ Action.NoOp
        pure s
    }
