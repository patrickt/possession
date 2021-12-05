{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.List.Pointed
  ( PL.PointedList (..),
    PL.next,
    PL.previous,
    moveTo,
    PL.find,
    focus,
    IsList (..),
  )
where

import Data.Foldable qualified
import Data.List.PointedList.Circular qualified as PL
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList (..))
import Optics

instance IsList (PL.PointedList a) where
  type Item (PL.PointedList a) = a
  fromList = fromMaybe (error "error: empty PointedList literal") . PL.fromList
  toList = Data.Foldable.toList

focus :: Lens' (PL.PointedList a) a
focus = lensVL PL.focus

moveTo :: Int -> PL.PointedList a -> PL.PointedList a
moveTo n pl = PL.moveN (n - PL.index pl) pl
