{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Data.Message
  ( Message (Message),
    fromText,
    contents,
    urgency,
    times,
    Urgency (..),
  )
where

import Data.Aeson.Exts
import Data.Generics.Product
import Data.Semigroup
import Data.Semigroup.Generic
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics

data Urgency = Info | Warning | Danger
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Message = Message {_contents :: Last Text, _urgency :: Max Urgency, _times :: Sum Int}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving Semigroup via GenericSemigroup Message

instance IsString Message where
  fromString s = Message (pure (fromString s)) (pure Info) 1

fromText :: Text -> Message
fromText t = Message (pure t) (pure Info) 1

contents :: Lens' Message Text
contents = field @"_contents" % coerced

urgency :: Lens' Message Urgency
urgency = field @"_urgency" % coerced

times :: Lens' Message Int
times = field @"_times" % coerced
