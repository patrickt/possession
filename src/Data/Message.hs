{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A message that should be passed to the rendering system,
-- with an associated urgency. Records the number of times
-- it has been coalesced with other messages to beautify
-- the display of many repeated messages.
module Data.Message
  ( Message (Message),
    fromText,
    mergeable,
    contents,
    Urgency (..),
    youSee,
    debug,
  )
where

import Data.Aeson.Exts
import Data.Function
import Data.Name (Name)
import Data.Name qualified as Name
import Data.Semigroup
import Data.Semigroup.Generic
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics
import qualified Data.Monoid
import Data.Monoid.Generic

data Urgency = Debug | Info | Warning | Danger
  deriving stock (Eq, Show, Bounded, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Message = Message
  { messageContents :: Data.Monoid.Last Text,
    messageUrgency :: Max Urgency,
    messageTimes :: Sum Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Semigroup) via GenericSemigroup Message
  deriving (Monoid) via GenericMonoid Message

instance IsString Message where
  fromString s = Message (pure (fromString s)) (pure Info) 1

makeFieldLabels ''Message

contents :: Message -> Text
contents m = m ^. #contents % coerced % non ""

mergeable :: Message -> Message -> Bool
mergeable = (==) `on` view #contents

fromText :: Text -> Message
fromText t = Message (pure t) (pure Info) 1

debug :: String -> Message
debug s = Message (pure (fromString s)) (pure Debug) 1

youSee :: Name -> Message
youSee n = Message (pure ("You see " <> Name.definiteArticle n <> " " <> Name.text n <> ".")) (pure Info) 1
