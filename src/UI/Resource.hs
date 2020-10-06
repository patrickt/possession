module UI.Resource
  ( Resource (..),
  )
where

data Resource
  = Modeline
  | Canvas
  | MainMenu
  | Readout
  | Unspecified
  deriving (Eq, Show, Ord)
