{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Optics.Tupled where

import Optics

class Tupled from tup | from -> tup, tup -> from where
  tupled :: Iso' from tup
