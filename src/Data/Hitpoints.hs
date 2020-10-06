module Data.Hitpoints (HP (..)) where

data HP = HP {current :: !Int, total :: !Int}
