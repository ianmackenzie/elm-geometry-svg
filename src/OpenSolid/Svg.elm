module OpenSolid.Svg
    exposing
        ( scene2d
        , point2d
        , lineSegment2d
        , triangle2d
        )

import String
import Html exposing (Html)
import Svg as Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.LineSegment.Types exposing (..)
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Triangle.Types exposing (..)
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Bounds.Types exposing (..)
import OpenSolid.Bounds.Interval as Interval


scene2d : Interval -> Interval -> List (Svg msg) -> Html msg
scene2d xInterval yInterval elements =
    let
        ( xMin, xMax ) =
            Interval.endpoints xInterval

        ( yMin, yMax ) =
            Interval.endpoints yInterval

        width =
            toString (Interval.width xInterval)

        height =
            toString (Interval.width yInterval)

        viewBox =
            String.join " " [ toString xMin, toString -yMax, width, height ]

        transform =
            "scale(1 -1)"
    in
        Svg.svg
            [ Attributes.width width
            , Attributes.height height
            , Attributes.viewBox viewBox
            ]
            [ Svg.g [ Attributes.transform transform ] elements ]


point2d : List (Attribute msg) -> Point2d -> Svg msg
point2d attributes point =
    let
        ( x, y ) =
            Point2d.coordinates point

        cx =
            Attributes.cx (toString x)

        cy =
            Attributes.cy (toString y)
    in
        Svg.circle (cx :: cy :: attributes) []


lineSegment2d : List (Attribute msg) -> LineSegment2d -> Svg msg
lineSegment2d attributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        commands =
            [ "M", toString x1, toString y1, "L", toString x2, toString y2 ]

        d =
            Attributes.d (String.join " " commands)
    in
        Svg.path (d :: attributes) []


triangle2d : List (Attribute msg) -> Triangle2d -> Svg msg
triangle2d attributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

        commands =
            [ "M"
            , toString x1
            , toString y1
            , "L"
            , toString x2
            , toString y2
            , "L"
            , toString x3
            , toString y3
            , "Z"
            ]

        d =
            Attributes.d (String.join " " commands)
    in
        Svg.path (d :: attributes) []
