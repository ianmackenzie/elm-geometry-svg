module Main exposing (..)

import Html exposing (Html)
import Svg as Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import OpenSolid.Bounds.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Types exposing (..)
import OpenSolid.LineSegment.Types exposing (..)
import OpenSolid.Svg as Svg
import OpenSolid.Triangle.Types exposing (..)
import OpenSolid.Triangle2d as Triangle2d


main : Html msg
main =
    let
        angles =
            List.map (\index -> degrees (6 * index)) [0..15]

        --Raw geometry
        points =
            List.map (Point2d.polar 300) angles

        lineSegments =
            List.map (LineSegment2d Point2d.origin) points

        firstTriangle =
            Triangle2d (Point2d 300 -10) (Point2d 320 0) (Point2d 300 10)

        rotatedTriangle angle =
            Triangle2d.rotateAround Point2d.origin angle firstTriangle

        triangles =
            List.map rotatedTriangle angles

        -- Drawing functions
        point2d =
            Svg.point2d [ Attributes.r "5", Attributes.fill "cornflowerblue" ]

        lineSegment2d =
            Svg.lineSegment2d [ Attributes.strokeWidth "0.5" ]

        triangle2d =
            Svg.triangle2d [ Attributes.fill "palegreen" ]

        -- SVG elements
        pointElements =
            List.map point2d points

        lineElements =
            List.map lineSegment2d lineSegments

        triangleElements =
            List.map triangle2d triangles

        originElement =
            Svg.point2d
                [ Attributes.r "5"
                , Attributes.strokeWidth "0.5"
                , Attributes.fill "white"
                ]
                Point2d.origin

        allElements =
            List.concat
                [ lineElements
                , triangleElements
                , pointElements
                , [ originElement ]
                ]

        -- Top level group
        defaultAttributes =
            [ Attributes.stroke "black"
            , Attributes.strokeWidth "1"
            ]

        topLevelGroup =
            Svg.g defaultAttributes allElements
    in
        Svg.scene2d (Interval -50 350) (Interval -50 350) [ topLevelGroup ]
