let Enemy = ./Enemy.dhall

let Collision = ./Collision.dhall

let Color = ./Color.dhall

let Strategy = ./Strategy.dhall

in    [ { name = "gibbering idiot"
        , glyph = "g"
        , onCollide = Collision.DoNothing
        , color = Color.Yellow
        , strategy = Strategy.Wander
        , canDrop = 5
        , yieldsXP = 3
        , hearing = 5
        }
      , { name = "wolf"
        , glyph = "C"
        , onCollide = Collision.Attack
        , color = Color.Grey
        , strategy = Strategy.FightOnSight
        , canDrop = 0
        , yieldsXP = 5
        , hearing = 20
        }
      , { name = "battle-scarred veteran"
        , glyph = "H"
        , onCollide = Collision.Attack
        , color = Color.Red
        , strategy = Strategy.FightOnSight
        , canDrop = 10
        , yieldsXP = 10
        , hearing = 15
        }
      , { name = "blink dog"
        , glyph = "d"
        , onCollide = Collision.Attack
        , color = Color.White
        , strategy = Strategy.FightOnSight
        , canDrop = 0
        , yieldsXP = 20
        , hearing = 15
        }
      ]
    : List Enemy
