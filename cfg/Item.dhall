let ItemProperty = ./ItemProperty.dhall

in  { name : Text
    , glyph : Text
    , color : ./Color.dhall
    , properties : List ItemProperty
    , value : Natural
    }
