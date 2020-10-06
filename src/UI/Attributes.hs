{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Attributes
  ( withStandard,
  )
where

import Brick qualified
import Graphics.Vty qualified as Vty

withStandard :: Brick.Widget n -> Brick.Widget n
withStandard = Brick.updateAttrMap (Brick.applyAttrMappings standard)

standard :: [(Brick.AttrName, Vty.Attr)]
standard =
  [ ("green", Brick.fg Vty.green),
    ("bold", Vty.withStyle Vty.defAttr Vty.bold)
  ]
