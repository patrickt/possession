let Enemy
    : Type
    = { name : Text
      , glyph : Text
      , color : Text
      , behavior : Text
      , canDrop : Natural
      }

in    [ { name = "gibbering idiot"
        , glyph = "g"
        , behavior = "attack"
        , color = "yellow"
        , canDrop = 5
        }
      , { name = "wolf"
        , glyph = "C"
        , behavior = "attack"
        , color = "gray"
        , canDrop = 0
        }
      ]
    : List Enemy
