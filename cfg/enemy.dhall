let Enemy
    : Type
    = { name : Text, glyph : Text, color : Text, behavior : Text }

in    [ { name = "gibbering idiot"
        , glyph = "g"
        , behavior = "attack"
        , color = "yellow"
        }
      , { name = "wolf", glyph = "C", behavior = "attack", color = "gray" }
      ]
    : List Enemy
