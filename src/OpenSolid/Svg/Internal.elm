module OpenSolid.Svg.Internal exposing (tip)

import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)


tip : { a | tipLength : Float, tipWidth : Float } -> Point2d -> Float -> Direction2d -> Triangle2d
tip { tipLength, tipWidth } basePoint length direction =
    let
        frame =
            Frame2d.with
                { originPoint = basePoint
                , xDirection = direction
                }

        tipPoint =
            Point2d.in_ frame ( length, 0 )

        tipBasePoint =
            Point2d.in_ frame ( length - tipLength, 0 )

        leftPoint =
            Point2d.in_ frame ( length - tipLength, tipWidth / 2 )

        rightPoint =
            Point2d.in_ frame ( length - tipLength, -tipWidth / 2 )
    in
    Triangle2d.fromVertices ( rightPoint, tipPoint, leftPoint )
