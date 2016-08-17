module OpenSolid.Svg
    exposing
        ( scene2d
        , point2d
        , lineSegment2d
        , triangle2d
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , placeIn
        )

import String
import Html exposing (Html)
import Svg as Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Frame2d as Frame2d
import OpenSolid.BoundingBox.Types exposing (..)
import OpenSolid.BoundingBox.BoundingBox2d as BoundingBox2d
import OpenSolid.LineSegment.Types exposing (..)
import OpenSolid.LineSegment.LineSegment2d as LineSegment2d
import OpenSolid.Triangle.Types exposing (..)
import OpenSolid.Triangle.Triangle2d as Triangle2d


scene2d : BoundingBox2d -> List (Svg msg) -> Html msg
scene2d boundingBox elements =
    let
        minX =
            BoundingBox2d.minX boundingBox

        maxX =
            BoundingBox2d.maxX boundingBox

        minY =
            BoundingBox2d.minY boundingBox

        maxY =
            BoundingBox2d.maxY boundingBox

        width =
            toString (maxX - minX)

        height =
            toString (maxY - minY)

        viewBox =
            String.join " " [ toString minX, toString -maxY, width, height ]

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


scaleAbout : Point2d -> Float -> Svg msg -> Svg msg
scaleAbout point scale =
    placeIn (Frame2d.scaleAbout point scale Frame2d.xy)


rotateAround : Point2d -> Float -> Svg msg -> Svg msg
rotateAround point angle =
    placeIn (Frame2d.rotateAround point angle Frame2d.xy)


translateBy : Vector2d -> Svg msg -> Svg msg
translateBy vector =
    placeIn (Frame2d.translateBy vector Frame2d.xy)


mirrorAcross : Axis2d -> Svg msg -> Svg msg
mirrorAcross axis =
    placeIn (Frame2d.mirrorAcross axis Frame2d.xy)


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
            String.join " " (List.map toString [ x1, y1, x2, y2, px, py ])

        transform =
            "matrix(" ++ components ++ ")"
    in
        Svg.g [ Attributes.transform transform ] [ element ]
