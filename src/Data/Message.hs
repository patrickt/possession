{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A message that should be passed to the rendering system,
-- with an associated urgency. Records the number of times
-- it has been coalesced with other messages to beautify
-- the display of many repeated messages.
module Data.Message
  ( Message (Message),
    fromText,
    Urgency (..),
  )
where

import Data.Aeson.Exts
import Data.Semigroup
import Data.Semigroup.Generic
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics

data Urgency = Info | Warning | Danger
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Message = Message
  { contents :: Data.Semigroup.Last Text,
    urgency :: Max Urgency,
    times :: Sum Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Semigroup) via GenericSemigroup Message

instance IsString Message where
  fromString s = Message (pure (fromString s)) (pure Info) 1

makeFieldLabelsWith noPrefixFieldLabels ''Message

fromText :: Text -> Message
fromText t = Message (pure t) (pure Info) 1
