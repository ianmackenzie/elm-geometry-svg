module Regions exposing (..)

import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Mesh as Mesh
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Rectangle2d as Rectangle2d
import OpenSolid.Region2d as Region2d
import OpenSolid.Svg as Svg
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
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
    Svg.g curveAttributes (List.map (Svg.curve2d tolerance []) boundaryCurves)


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
            Mesh.faces mesh |> List.map Triangle2d.fromVertices
    in
    Svg.g [] (List.map drawMeshTriangle triangles)


main : Html Never
main =
    let
        boundingBox =
            BoundingBox2d.with
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
            Region2d.roundedRectangle rectangle 100

        boundaries =
            drawBoundaries tolerance region

        mesh =
            drawMesh tolerance region
    in
    Svg.render2d boundingBox (Svg.g [] [ mesh, boundaries ])
