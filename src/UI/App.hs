{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.App (app) where

import Brick qualified
import Brick.AttrMap qualified
import Control.Concurrent (killThread)
import Control.Effect.Broker qualified as Broker
import Control.Monad.IO.Class
import Data.Typeable
import Game.Action (Action, Dest (..))
import Game.Action qualified as Action
import Graphics.Vty qualified as Vty
import Optics
import UI.Canvas qualified as Canvas
import UI.Render
import UI.Resource qualified as UI (Resource)
import UI.Responder (Responder)
import UI.Responder qualified as Responder
import UI.Responder.Chain (castTo)
import UI.Responder.Chain qualified as Responder
import UI.State qualified as State
import UI.State qualified as UI (State)
import UI.Widgets.Modeline qualified as Modeline
import UI.Widgets.Toplevel (Toplevel)

draw :: UI.State -> [Brick.Widget UI.Resource]
draw s = render (s ^. #renderStack) []

handleEvent ::
  UI.State ->
  Brick.BrickEvent UI.Resource (Action 'UI) ->
  Brick.EventM UI.Resource (Brick.Next UI.State)
handleEvent s evt = case evt of
  Brick.VtyEvent vty -> handleVty s vty
  Brick.AppEvent cmd -> handleApp s cmd
  _ -> Brick.continue s

handleVty :: UI.State -> Vty.Event -> Brick.EventM UI.Resource (Brick.Next UI.State)
handleVty s vty =
  let first = s ^. #renderStack % Responder.first
      inp = Responder.translate vty first
      next fn = Brick.continue (fn s)
      go act = case act of
        Responder.Nil -> next id
        a `Responder.Then` b -> go a *> go b
        Responder.Push r ->
          next (#renderStack %~ Responder.push r)
        Responder.Pop -> do
          next (#renderStack %~ Responder.pop)
        Responder.Update a ->
          next (#renderStack % Responder.first .~ a)
        Responder.Broadcast it -> do
          liftIO
            . Broker.runBroker (s ^. #brokerage)
            . Broker.pushAction
            $ it
          next id
        Responder.Terminate -> do
          liftIO . killThread . view #gameThread $ s
          Brick.halt s
   in go (Responder.onSend inp (s ^. #latestInfo) first)

handleApp :: UI.State -> Action dest -> Brick.EventM UI.Resource (Brick.Next UI.State)
handleApp s cmd = Brick.continue $ case cmd of
  Action.Start -> s
  Action.Redraw canv ->
    propagate @Toplevel (#canvas %~ Canvas.update canv) s
  Action.Update info ->
    -- TODO: I don't like this double-info thing; can we make the sidebar not store it?
    -- Yes, I think we can, since we pass in the info in onSend now.
    s & #latestInfo .~ info
      & propagate @Toplevel (#sidebar % #info .~ info)
  Action.Notify msg -> do
    let lastMessage = State.firstResponder % castTo @Toplevel % #modeline % #messages % _last
    let previous = s ^? lastMessage % #contents
    let shouldCoalesce = previous == Just (msg ^. #contents)
    case (previous, shouldCoalesce) of
      (Just _, True) -> s & lastMessage % #times %~ (+ 1)
      _ -> propagate @Toplevel (#modeline %~ Modeline.update msg) s
  _ -> s

propagate :: (Renderable a, Responder a, Typeable a) => (a -> a) -> UI.State -> UI.State
propagate fn s = s & #renderStack %~ Responder.propagate fn

app :: Brick.App UI.State (Action 'UI) UI.Resource
app =
  Brick.App
    { Brick.appDraw = draw,
      Brick.appChooseCursor = Brick.showFirstCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appAttrMap = const (Brick.AttrMap.attrMap Vty.defAttr []),
      Brick.appStartEvent = \s -> do
        liftIO
          . Broker.runBroker (s ^. #brokerage)
          . Broker.pushAction
          $ Action.Start
        pure s
    }
