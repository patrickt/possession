{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UI.Responder
  ( Responder (..),
    SomeResponder (..),
    Chain (..),
    Response (..),
    first
  )
where

import Data.List.NonEmpty (NonEmpty(..))
import UI.Input (Input)
import UI.Render (Renderable (..))

data Response a
  = Push SomeResponder
  | Update a
  | Pop
  | Terminate

class Responder a where
  onSend :: Input -> a -> Response a

newtype SomeResponder = SomeResponder (forall x. (Renderable x, Responder x) => x)

newtype Chain = Chain (NonEmpty SomeResponder)

first :: Chain -> SomeResponder
first (Chain (x :| _)) = x
