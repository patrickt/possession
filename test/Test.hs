{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Hedgehog
import Control.Monad
-- import Hedgehog.Gen qualified as Gen
-- import Hedgehog.Range qualified as Range

main :: IO ()
main = void (checkParallel $$(discover))
