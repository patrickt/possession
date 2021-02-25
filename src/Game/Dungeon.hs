{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Game.Dungeon where

import Control.Comonad
import Data.Bits
import Data.Word

-- | A @Store s a@ describes some "test" that takes a configuration @s@
-- and will produce a value of type @a@, where we also have some ambient
-- initial configuration of type @s@ that is known with which we could
-- start the experiment.
data Store s a = Store (s -> a) s
  deriving (Functor)

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

-- | The 'experiment' combinator describes a 'Store' completely. It lets us
-- explore variations on the initial conditions of our test, by providing
-- a functorial preprocessing step applied before the extraction process
-- takes place.
--
-- >>> experiment @[] :: (s -> [s]) -> Store s a -> [a]
-- >>> experiment @Maybe :: (s -> Maybe s) -> Store s a -> Maybe a
-- >>> experiment @IO :: (s -> IO s) -> Store s a -> IO a
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store g s) = fmap g (f s)

seek :: (s -> s) -> Store s a -> Store s a
seek f = experiment (Store f)

rule :: Num s => Word8 -> Store s Bool -> Bool
rule w (Store f s) =
  let b2i x = if x then 1 else 0
   in testBit w $
        setBit (b2i (f (s + 1))) $
          setBit (b2i (f s)) $
            setBit (b2i (f (s - 1))) 0

slowLoop :: (Store s a -> a) -> Store s a -> [Store s a]
slowLoop f = iterate (extend f)

-- show
window :: (Enum s, Num s) => s -> s -> Store s a -> [a]
window l h = experiment $ \s -> [s - l .. s + h]

xo :: Bool -> Char
xo True = 'X'
xo False = ' '

main =
  mapM_ (putStrLn . map xo . window 50 0) $
    take 50 $ slowLoop (rule 110) $ Store (== 0) 12
