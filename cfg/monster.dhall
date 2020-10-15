let Monster
    : Type
    = { name : Text, glyph : Text, hp : Natural, color : Text }

in    [ { name = "gibbering idiot", glyph = "g", hp = 5, color = "yellow" }
      , { name = "wolf", glyph = "C", hp = 10, color = "gray" }
      ]
    : List Monster
