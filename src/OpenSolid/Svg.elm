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
import OpenSolid.LineSegment.Types exposing (..)
import OpenSolid.Triangle.Types exposing (..)
import OpenSolid.Bounds.Types exposing (..)


scene2d : Interval -> Interval -> List (Svg msg) -> Html msg
scene2d (Interval xMin xMax) (Interval yMin yMax) elements =
    let
        width =
            toString (xMax - xMin)

        height =
            toString (yMax - yMin)

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
point2d attributes (Point2d x y) =
    let
        cx =
            Attributes.cx (toString x)

        cy =
            Attributes.cy (toString y)
    in
        Svg.circle (cx :: cy :: attributes) []


lineSegment2d : List (Attribute msg) -> LineSegment2d -> Svg msg
lineSegment2d attributes (LineSegment2d p1 p2) =
    let
        (Point2d x1 y1) =
            p1

        (Point2d x2 y2) =
            p2

        commands =
            [ "M", toString x1, toString y1, "L", toString x2, toString y2 ]

        d =
            Attributes.d (String.join " " commands)
    in
        Svg.path (d :: attributes) []


triangle2d : List (Attribute msg) -> Triangle2d -> Svg msg
triangle2d attributes (Triangle2d p1 p2 p3) =
    let
        (Point2d x1 y1) =
            p1

        (Point2d x2 y2) =
            p2

        (Point2d x3 y3) =
            p3

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
