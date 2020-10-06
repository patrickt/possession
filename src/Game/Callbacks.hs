module Game.Callbacks where

data Collision
  = Invalid
  | Attack

data Callbacks = Callbacks
  { onCollision :: Collision
  }

hostile :: Callbacks
hostile = Callbacks (Attack)
