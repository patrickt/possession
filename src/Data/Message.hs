{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Data.Message
  ( Message (Message),
    contents,
    urgency,
    times,
    Urgency(..),
  )
where

import Data.Generics.Product
import Data.Semigroup
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics

data Urgency = Info | Warning | Danger
  deriving (Eq, Show, Ord)

data Message = Message {_contents :: Last Text, _urgency :: Max Urgency, _times :: Sum Int}
  deriving (Generic)

instance Semigroup Message where
  Message a b c <> Message d e f = Message (a <> d) (b <> e) (c <> f)

instance IsString Message where
  fromString s = Message (pure (fromString s)) (pure Info) 1

contents :: Lens' Message Text
contents = field @"_contents" % coerced

urgency :: Lens' Message Urgency
urgency = field @"_urgency" % coerced

times :: Lens' Message Int
times = field @"_times" % coerced
