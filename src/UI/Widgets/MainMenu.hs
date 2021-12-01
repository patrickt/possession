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
import UI.Render (Renderable (..), runDraw)
import UI.Resource qualified as Resource
import UI.Responder
import Game.Action (Action(..))
import UI.Event
import Game.Info (Info)
import Control.Effect.Reader

data Choice
  = NewGame
  | Resume
  | Load
  | Save
  | About
  | Quit
  deriving (Eq, Ord, Show, Enum)

instance Renderable Choice where
  draw = pure . go where
    go NewGame = Brick.txt "New Game"
    go other = Brick.txt . fromString . show $ other

newtype MainMenu = MainMenu
  { mainMenuChoices :: PointedList Choice
  }
  deriving stock (Show, Generic)

makeFieldLabels ''MainMenu

initial :: MainMenu
initial = MainMenu [NewGame, About, Quit]

inGame :: MainMenu
inGame = MainMenu [Resume, Save, Load, About, Quit]

selected :: Lens' MainMenu Choice
selected = #choices % Pointed.focus

instance Renderable MainMenu where
  draw x = do
    i <- ask @Info
    let
      toForm = Form.newForm [list]
      list = Form.listField allChoices (toLensVL (selected % re (non NewGame))) drawItem 8 Resource.MainMenu
      allChoices = Vector.fromList . toList . view #choices
      drawItem on = Brick.hCenter . (if on then Brick.border else id) . runDraw i
    pure . Form.renderForm . toForm $ x


instance Responder (Maybe MainMenu) where
  respondTo = up <|> down <|> selection
    where
      up = overState (keypress Vty.KUp) (fmap (over #choices Pointed.previous))
      down = overState (keypress Vty.KDown) (fmap (over #choices Pointed.next))
      selection = upon $ \a -> case a ^? _Just % selected of
           Just NewGame -> pure Nothing
           Just Resume -> pure Nothing
           Just Load -> Nothing `emitting` LoadState
           Just Save -> Nothing `emitting` SaveState
           Just Quit -> Nothing `emitting` Terminate
           _ -> empty
