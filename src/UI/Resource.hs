module UI.Resource
  ( Resource (..),
  )
where

data Resource
  = Modeline
  | Canvas
  | MainMenu
  | Unspecified
  deriving (Eq, Show, Ord)
