{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

module UI.Responder
  ( Responder (..),
    accept,
    try,
    runResponder,
    propagateResponse,
    andThen,
    findActions,
  )
where

import Data.Functor.Foldable
import Optics
import Data.Fix hiding (cata)
import qualified Graphics.Vty as Vty
import Game.Action (GameAction)
import Data.Functor.Classes

type Response a = Fix (Path a)

class Responder a where
  respondTo :: Vty.Event -> a -> Response a

instance Responder () where respondTo = mempty

data Path a r where
  Ok :: a -> Path a r
  Fail :: Path a r
  Try :: (k `Is` A_Fold, k `Is` A_Setter, Responder child) => Optic' k is a child -> Path a r
  Perform :: GameAction -> r -> Path a r
  Alt :: r -> r -> Path a r

instance (Show a) => Show1 (Path a) where
  liftShowsPrec showp _ n = \case
    Ok a -> showString "Ok " . shows a
    Fail -> showString "Fail"
    Try _ -> showString "try"
    Perform x a -> showString "Perform " . shows x . showParen (n > 7) (showp (succ n) a)
    Alt l r -> showString "Alt " . showParen (n > 7) (showp (succ n) l) . showChar ' ' . showParen (n > 7) (showp (succ n) r)

deriving stock instance Functor (Path a)

instance Semigroup (Response a) where
  a <> Fix Fail = a
  Fix Fail <> a = a
  a <> b = Fix (Alt a b)

instance Monoid (Response a) where mempty = Fix Fail

accept :: a -> Response a
accept = Fix . Ok

try :: (k `Is` A_Fold, k `Is` A_Setter, Responder child) => Optic' k is a child -> Response a
try = Fix . Try

andThen :: GameAction -> Response a -> Response a
andThen act = Fix . Perform act

runResponder :: Responder a => Vty.Event -> a -> a
runResponder e a = propagateResponse e (respondTo e a) a

findActions :: forall a . Response a -> a -> [GameAction]
findActions a = para go a ([] :: [GameAction]) where
  go :: Path a (Response a, [GameAction] -> a -> [GameAction]) -> [GameAction] -> a -> [GameAction]
  go p acts val = case p of
    Perform act rest -> act : snd rest acts val
    Alt (Fix (Try optic), it) alt -> if has optic val then it acts val else snd alt acts val
    Alt (Fix Fail, _) alt -> snd alt acts val
    Alt rest _ -> snd rest acts val
    _ -> acts


propagateResponse :: forall a . Vty.Event -> Response a -> a -> a
propagateResponse e = para go
  where
    go :: Path a (Response a, a -> a) -> a -> a
    go path = case path of
      Ok v -> const v
      Fail -> id
      Try optic -> over optic (runResponder e)
      Alt (Fix (Try optic), it) alt -> \val -> if has optic val then it val else snd alt val
      Alt (Fix Fail, _) x -> snd x
      Alt x _ -> snd x
      Perform _ x -> snd x
