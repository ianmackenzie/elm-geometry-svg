module Regions exposing (..)

import Html exposing (Html)
import OpenSolid.Curve2d as Curve2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Mesh as Mesh
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Rectangle2d as Rectangle2d
import OpenSolid.Region2d as Region2d
import OpenSolid.Svg as Svg
import OpenSolid.Triangle2d as Triangle2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes


drawBoundaries : Float -> Region2d -> Svg msg
drawBoundaries tolerance region =
    let
        boundaryCurves =
            Region2d.boundaries region

        curveAttributes =
            [ Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            , Attributes.stroke "black"
            ]
    in
    Svg.g curveAttributes (List.map (Svg.curve2d [] tolerance) boundaryCurves)


drawMeshTriangle : Triangle2d -> Svg msg
drawMeshTriangle triangle =
    let
        fillColor =
            if Triangle2d.counterclockwiseArea triangle >= 0.0 then
                "lightgrey"
            else
                "darkgrey"

        attributes =
            [ Attributes.strokeLinejoin "round"
            , Attributes.fill fillColor
            , Attributes.stroke "black"
            , Attributes.strokeWidth "0.5"
            ]
    in
    Svg.triangle2d attributes triangle


drawMesh : Float -> Region2d -> Svg msg
drawMesh tolerance region =
    let
        mesh =
            Region2d.toMesh tolerance region

        triangles =
            Mesh.faces mesh |> List.map Triangle2d
    in
    Svg.g [] (List.map drawMeshTriangle triangles)


roundedRectangle : Rectangle2d -> Float -> Region2d
roundedRectangle rectangle cornerRadius =
    let
        ( width, height ) =
            Rectangle2d.dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2

        frame =
            Rectangle2d.axes rectangle

        xAxis =
            Frame2d.xAxis frame

        yAxis =
            Frame2d.yAxis frame

        cornerCenter =
            Point2d.in_ frame
                ( halfWidth - cornerRadius
                , halfHeight - cornerRadius
                )

        cornerStart =
            Point2d.in_ frame ( halfWidth, halfHeight - cornerRadius )

        topRightCorner =
            Region2d.revolutionWith
                { start = Region2d.interior
                , end = Region2d.interior
                , inside = Region2d.interior
                , outside = Region2d.exterior
                }
                (Curve2d.lineSegment ( cornerCenter, cornerStart ))
                cornerCenter
                (degrees 90)

        rightRectangle =
            Region2d.fromRectangleWith
                { left = Region2d.interior
                , right = Region2d.exterior
                , top = Region2d.interior
                , bottom = Region2d.interior
                }
                (Rectangle2d.in_ frame
                    { minX = halfWidth - cornerRadius
                    , maxX = halfWidth
                    , minY = -halfHeight + cornerRadius
                    , maxY = halfHeight - cornerRadius
                    }
                )

        topRectangle =
            Region2d.fromRectangleWith
                { left = Region2d.interior
                , right = Region2d.interior
                , top = Region2d.exterior
                , bottom = Region2d.interior
                }
                (Rectangle2d.in_ frame
                    { minX = -halfWidth + cornerRadius
                    , maxX = halfWidth - cornerRadius
                    , minY = halfHeight - cornerRadius
                    , maxY = halfHeight
                    }
                )

        leftRectangle =
            rightRectangle |> Region2d.mirrorAcross yAxis

        bottomRectangle =
            topRectangle |> Region2d.mirrorAcross xAxis

        topLeftCorner =
            topRightCorner |> Region2d.mirrorAcross yAxis

        bottomRightCorner =
            topRightCorner |> Region2d.mirrorAcross xAxis

        bottomLeftCorner =
            topLeftCorner |> Region2d.mirrorAcross xAxis

        innerRectangle =
            Region2d.fromRectangleWith
                { left = Region2d.interior
                , right = Region2d.interior
                , top = Region2d.interior
                , bottom = Region2d.interior
                }
                (Rectangle2d.in_ frame
                    { minX = -halfWidth + cornerRadius
                    , maxX = halfWidth - cornerRadius
                    , minY = -halfHeight + cornerRadius
                    , maxY = halfHeight - cornerRadius
                    }
                )
    in
    Region2d.fuse
        [ topLeftCorner
        , topRectangle
        , topRightCorner
        , leftRectangle
        , innerRectangle
        , rightRectangle
        , bottomLeftCorner
        , bottomRectangle
        , bottomRightCorner
        ]


main : Html Never
main =
    let
        boundingBox =
            BoundingBox2d
                { minX = 0
                , maxX = 800
                , minY = 0
                , maxY = 600
                }

        rectangle =
            Rectangle2d.with { minX = 100, minY = 100, maxX = 700, maxY = 500 }

        tolerance =
            1

        region =
            roundedRectangle rectangle 100

        boundaries =
            drawBoundaries tolerance region

        mesh =
            drawMesh tolerance region
    in
    Svg.render2d boundingBox (Svg.g [] [ mesh, boundaries ])
