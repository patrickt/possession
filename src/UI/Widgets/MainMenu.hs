{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UI.Widgets.MainMenu
  ( MainMenu (MainMenu),
    initial,
    inGame,
  )
where

import Brick qualified
import Brick.Forms qualified as Form
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Data.Foldable
import Data.List.Pointed (PointedList)
import Data.List.Pointed qualified as Pointed
import Data.String
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Graphics.Vty qualified as Vty
import Optics
import UI.Render (Renderable (..))
import UI.Resource qualified as Resource
import UI.Responder
import Game.Action (Action(..))

data Choice
  = NewGame
  | Resume
  | Load
  | Save
  | About
  | Quit
  deriving (Eq, Ord, Show, Enum)

instance Renderable Choice where
  draw NewGame = Brick.txt "New Game"
  draw other = Brick.txt . fromString . show $ other

newtype MainMenu = MainMenu
  { choices :: PointedList Choice
  }
  deriving stock (Show, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''MainMenu

initial :: MainMenu
initial = MainMenu [NewGame, About, Quit]

inGame :: MainMenu
inGame = MainMenu [Resume, Save, Load, About, Quit]

selected :: Lens' MainMenu Choice
selected = #choices % Pointed.focus

instance Renderable MainMenu where
  draw = Form.renderForm . toForm
    where
      toForm = Form.newForm [list]
      list = Form.listField allChoices (toLensVL (selected % re (non NewGame))) drawItem 8 Resource.MainMenu
      allChoices = Vector.fromList . toList . view #choices
      drawItem on = Brick.hCenter . (if on then Brick.border else id) . draw

instance Responder (Maybe MainMenu) where
  respondTo (Vty.EvKey k _) (Just mm) = case k of
    Vty.KUp -> accept . Just . over #choices Pointed.previous $ mm
    Vty.KDown -> accept . Just . over #choices Pointed.next $ mm
    Vty.KEnter -> case mm ^. selected of
      NewGame -> accept Nothing
      Resume -> accept Nothing
      Load -> LoadState `andThen` accept Nothing
      Save -> LoadState `andThen` accept Nothing
      Quit -> Terminate `andThen` accept (Just mm)
      _ -> mempty
    _ -> mempty
  respondTo _ _ = mempty

-- instance Responder MainMenu where
--   translate (Vty.EvKey k _) _ = case k of
--     Vty.KUp -> Input.Up
--     Vty.KDown -> Input.Down
--     Vty.KEnter -> Input.Confirm
--     _ -> Input.None
--   translate _ _ = Input.None

--   onSend inp _inf s =
--     case inp of
--       Input.Up -> Update (s & #choices %~ Pointed.previous)
--       Input.Down -> Update (s & #choices %~ Pointed.next)
--       Input.Confirm -> case s ^. selected of
--         NewGame -> Pop
--         Quit -> Terminate
--         Save -> Broadcast SaveState <> Pop
--         Load -> Broadcast LoadState <> Pop
--         Resume -> Pop
--         _ -> Nil
--       _ -> Nil

-- instance CanHandle MainMenu where
--   handleEvent (Vty.EvKey k _) mm = case k of
--     Vty.KUp -> mm & #choices %~ Pointed.previous & updating
--     Vty.KDown -> mm & #choices %~ Pointed.next & updating
--     Vty.KEnter -> case mm ^. selected of
--       NewGame -> Ok Pop
--       Quit -> Ok Terminate
--       Save -> Ok (Broadcast SaveState <> Pop)
--       Load -> Ok (Broadcast LoadState <> Pop)
--       Resume -> Ok Pop
--       _ -> Fail
--     _ -> Fail
--   handleEvent _ _ = Fail

-- render' :: Bool -> Choice -> Brick.Widget Resource.Resource
-- render' isOn =
--   Brick.hCenter
--     . (if isOn then Brick.border else id)
--     . head
--     . flip render []

-- form :: MainMenu -> Form.Form MainMenu e Resource.Resource
-- form = Form.newForm [theList]
--   where
--     theList =
--       Form.listField
--         (fromList . toList . view #choices)
--         (toLensVL (selected % re (non NewGame)))
--         render'
--         8
--         Resource.MainMenu

-- instance Renderable MainMenu where render s stack = Form.renderForm (form s) : stack
