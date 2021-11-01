let Enemy = ./Enemy.dhall

let Collision = ./Collision.dhall

let Color = ./Color.dhall

let Strategy = ./Strategy.dhall

in    [ { name = "gibbering idiot"
        , glyph = "g"
        , onCollide = Collision.Attack
        , color = Color.Yellow
        , strategy = Strategy.Wander
        , canDrop = 5
        , yieldsXP = 3
        }
      , { name = "wolf"
        , glyph = "C"
        , onCollide = Collision.Attack
        , color = Color.White
        , strategy = Strategy.FightOnSight
        , canDrop = 0
        , yieldsXP = 5
        }
      ]
    : List Enemy
