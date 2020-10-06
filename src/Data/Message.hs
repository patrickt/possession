{-# LANGUAGE DeriveGeneric #-}

module Data.Message where

import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)

data Urgency = Info | Warning | Danger

data Message = Message {contents :: Text, urgency :: Urgency}
  deriving (Generic)

instance IsString Message where
  fromString s = Message (fromString s) Info
