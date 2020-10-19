{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Optics.Tupled where

import Optics

class Tupled from tup | from -> tup where
  tupled :: Iso' from tup
