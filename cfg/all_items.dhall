let Item = ./Item.dhall

let Color = ./Color.dhall

let ItemProperty = ./ItemProperty.dhall

in  [ { name = "shovel"
      , glyph = "s"
      , color = Color.Brown
      , properties = ItemProperty.AllowsDig
      , value = 10
      }
    ]
