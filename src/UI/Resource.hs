module UI.Resource
  ( Resource (..),
  )
where

data Resource
  = Modeline
  | Canvas
  | MainMenu
  | Look
  | Readout
  | Unspecified
  deriving (Eq, Show, Ord)
