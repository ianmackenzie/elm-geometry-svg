module Utils.Color exposing (convert)

import Color


type alias Rgb =
    { red : Int
    , green : Int
    , blue : Int
    }


convert : Rgb -> Color.Color
convert { red, green, blue } =
    Color.rgb255 red green blue
