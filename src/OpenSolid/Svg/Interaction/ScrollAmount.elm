module OpenSolid.Svg.Interaction.ScrollAmount
    exposing
        ( ScrollAmount(..)
        , inLines
        , inPages
        , inPixels
        , isPositive
        )


type ScrollAmount
    = Pixels Float
    | Lines Float
    | Pages Float


isPositive : ScrollAmount -> Bool
isPositive scrollAmount =
    case scrollAmount of
        Pixels pixels ->
            pixels > 0

        Lines lines ->
            lines > 0

        Pages pages ->
            pages > 0


inPixels : ScrollAmount -> Float
inPixels scrollAmount =
    case scrollAmount of
        Pixels pixels ->
            pixels

        Lines lines ->
            lines * 40

        Pages pages ->
            pages * 800


inLines : ScrollAmount -> Float
inLines scrollAmount =
    case scrollAmount of
        Pixels pixels ->
            pixels / 40

        Lines lines ->
            lines

        Pages pages ->
            pages * 20


inPages : ScrollAmount -> Float
inPages scrollAmount =
    case scrollAmount of
        Pixels pixels ->
            pixels / 800

        Lines lines ->
            lines / 20

        Pages pages ->
            pages
