module OpenSolid.Svg
    exposing
        ( point2d
        , lineSegment2d
        , triangle2d
        , polyline2d
        , polygon2d
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        )

import String
import Html exposing (Html)
import Svg as Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Polyline2d as Polyline2d
import OpenSolid.Polygon2d as Polygon2d


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


coordinatesString : Point2d -> String
coordinatesString point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        toString x ++ "," ++ toString y


pointsAttribute : List Point2d -> Attribute msg
pointsAttribute points =
    Attributes.points (String.join " " (List.map coordinatesString points))


lineSegment2d : List (Attribute msg) -> LineSegment2d -> Svg msg
lineSegment2d attributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment
    in
        Svg.polyline (pointsAttribute [ p1, p2 ] :: attributes) []


triangle2d : List (Attribute msg) -> Triangle2d -> Svg msg
triangle2d attributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
        Svg.polygon (pointsAttribute [ p1, p2, p3 ] :: attributes) []


polyline2d : List (Attribute msg) -> Polyline2d -> Svg msg
polyline2d attributes polyline =
    let
        vertices =
            Polyline2d.vertices polyline
    in
        Svg.polyline (pointsAttribute vertices :: attributes) []


polygon2d : List (Attribute msg) -> Polygon2d -> Svg msg
polygon2d attributes polygon =
    let
        vertices =
            Polygon2d.vertices polygon
    in
        Svg.polygon (pointsAttribute vertices :: attributes) []


scaleAbout : Point2d -> Float -> Svg msg -> Svg msg
scaleAbout point scale element =
    let
        ( px, py ) =
            Point2d.coordinates (Point2d.scaleAbout point scale Point2d.origin)

        components =
            List.map toString [ scale, 0, 0, scale, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
        Svg.g [ Attributes.transform transform ] [ element ]


rotateAround : Point2d -> Float -> Svg msg -> Svg msg
rotateAround point angle =
    placeIn (Frame2d.rotateAround point angle Frame2d.xy)


translateBy : Vector2d -> Svg msg -> Svg msg
translateBy vector =
    placeIn (Frame2d.translateBy vector Frame2d.xy)


mirrorAcross : Axis2d -> Svg msg -> Svg msg
mirrorAcross axis =
    placeIn (Frame2d.mirrorAcross axis Frame2d.xy)


relativeTo : Frame2d -> Svg msg -> Svg msg
relativeTo frame =
    placeIn (Frame2d.relativeTo frame Frame2d.xy)


placeIn : Frame2d -> Svg msg -> Svg msg
placeIn frame element =
    let
        ( px, py ) =
            Point2d.coordinates (Frame2d.originPoint frame)

        ( x1, y1 ) =
            Direction2d.components (Frame2d.xDirection frame)

        ( x2, y2 ) =
            Direction2d.components (Frame2d.yDirection frame)

        components =
            List.map toString [ x1, y1, x2, y2, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
        Svg.g [ Attributes.transform transform ] [ element ]
