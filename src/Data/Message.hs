{-# LANGUAGE DeriveGeneric #-}

module Data.Message where

import Data.Text (Text)
import Data.String
import GHC.Generics (Generic)

data Urgency = Info | Warning | Danger

data Message = Message { contents :: Text, urgency :: Urgency }
  deriving Generic

instance IsString Message where
  fromString s = Message (fromString s) Info
