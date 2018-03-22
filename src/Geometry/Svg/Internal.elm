module Geometry.Svg.Internal exposing (tip)

import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)


tip : { a | tipLength : Float, tipWidth : Float } -> Point2d -> Float -> Direction2d -> Triangle2d
tip { tipLength, tipWidth } basePoint length direction =
    let
        localFrame =
            Frame2d.with
                { originPoint = basePoint
                , xDirection = direction
                }

        tipPoint =
            Point2d.fromCoordinatesIn localFrame
                ( length, 0 )

        tipBasePoint =
            Point2d.fromCoordinatesIn localFrame
                ( length - tipLength, 0 )

        leftPoint =
            Point2d.fromCoordinatesIn localFrame
                ( length - tipLength, tipWidth / 2 )

        rightPoint =
            Point2d.fromCoordinatesIn localFrame
                ( length - tipLength, -tipWidth / 2 )
    in
    Triangle2d.fromVertices ( rightPoint, tipPoint, leftPoint )
