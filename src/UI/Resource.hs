module UI.Resource
  ( Resource (..),
  )
where

-- | Names visible widgets in the UI.
data Resource
  = Modeline
  | Canvas
  | MainMenu
  | Look
  | Readout
  | Unspecified
  deriving (Eq, Show, Ord)
