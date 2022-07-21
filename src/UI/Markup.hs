{-# LANGUAGE TemplateHaskell #-}
module UI.Markup
  ( markup
  , text
  , shown
  , fg
  , bold
  , (&)
  , (&&)
  ) where

import Prelude hiding ((&&))
import TextShow (TextShow, showt)
import Data.Functor.Foldable hiding (Cons)
import Data.Functor.Foldable.TH
import Brick qualified
import Graphics.Vty qualified as Vty
import Data.Function ((&))
import UI.Resource (Widget)
import Data.Text (Text)
import Data.Color
import Data.String (IsString (..))
import Data.Text.Lazy qualified as Text.Lazy

data Markup
  = Cons Markup !Markup
  | Text !Text
  | Bold !Text
  | Fg !Color !Text

makeBaseFunctor ''Markup

instance Semigroup Markup where (<>) = Cons

instance IsString Markup where
  fromString = text . fromString

markup :: Markup -> Widget
markup = Brick.raw . cata go
  where
    from = Text.Lazy.fromStrict
    go = \case
      ConsF l r -> Vty.horizJoin l r
      TextF t -> Vty.text Vty.defAttr (from t)
      BoldF t -> Vty.text (Vty.defAttr `Vty.withStyle` Vty.bold) (from t)
      FgF c t -> Vty.text (Vty.defAttr `Vty.withForeColor` toVty c) (from t)

text :: Text -> Markup
text = Text

shown :: TextShow t => t -> Markup
shown = text . showt

fg :: Color -> Text -> Markup
fg = Fg

bold :: Text -> Markup
bold = Bold

(&&) :: TextShow a => a -> (Text -> b) -> b
a && b = b (showt a)
