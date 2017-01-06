module DocumentationExamples exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Axis2d as Axis2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.SketchPlane3d as SketchPlane3d
import Html exposing (Html)
import Html.Attributes


viewPlane : SketchPlane3d
viewPlane =
    Frame3d.xyz
        |> Frame3d.rotateAroundOwn Axis3d.z (degrees 30)
        |> Frame3d.rotateAroundOwn Axis3d.y (degrees -30)
        |> Frame3d.yzSketchPlane


arrow :
    List (Svg.Attribute Never)
    -> { tipLength : Float, tipWidth : Float }
    -> Point2d
    -> Vector2d
    -> Svg Never
arrow attributes { tipLength, tipWidth } point vector =
    case Vector2d.lengthAndDirection vector of
        Just ( length, direction ) ->
            let
                frame =
                    Frame2d
                        { originPoint = point
                        , xDirection = direction
                        , yDirection = Direction2d.perpendicularTo direction
                        }

                place =
                    Point2d.placeIn frame

                tipPoint =
                    place (Point2d ( length, 0 ))

                tipBasePoint =
                    place (Point2d ( length - tipLength, 0 ))

                leftPoint =
                    place (Point2d ( length - tipLength, tipWidth / 2 ))

                rightPoint =
                    place (Point2d ( length - tipLength, -tipWidth / 2 ))

                stem =
                    LineSegment2d ( point, tipBasePoint )

                tip =
                    Triangle2d ( rightPoint, tipPoint, leftPoint )
            in
                Svg.g attributes
                    [ Svg.lineSegment2d [] stem
                    , Svg.triangle2d [] tip
                    ]

        Nothing ->
            Svg.text ""


vector2d : Point2d -> Vector2d -> Svg Never
vector2d =
    arrow [ Attributes.fill "black" ] { tipLength = 6, tipWidth = 5 }


vector3d : Point3d -> Vector3d -> Svg Never
vector3d point vector =
    vector2d (Point3d.projectInto viewPlane point)
        (Vector3d.projectInto viewPlane vector)


direction2d : Point2d -> Direction2d -> Svg Never
direction2d point direction =
    arrow [ Attributes.fill "white" ]
        { tipLength = 5, tipWidth = 5 }
        point
        (Direction2d.times 25 direction)


direction3d : Point3d -> Direction3d -> Svg Never
direction3d point direction =
    arrow [ Attributes.fill "white" ]
        { tipLength = 5, tipWidth = 5 }
        (Point3d.projectInto viewPlane point)
        (Vector3d.projectInto viewPlane (Direction3d.times 25 direction))


originPoint2d : Point2d -> Svg Never
originPoint2d point =
    Svg.circle2d [ Attributes.fill "white" ]
        (Circle2d { centerPoint = point, radius = 1.5 })


originPoint3d : Point3d -> Svg Never
originPoint3d =
    Point3d.projectInto viewPlane >> originPoint2d


frame2d : Frame2d -> Svg Never
frame2d frame =
    let
        originPoint =
            Frame2d.originPoint frame
    in
        Svg.g []
            [ direction2d originPoint (Frame2d.xDirection frame)
            , direction2d originPoint (Frame2d.yDirection frame)
            , originPoint2d originPoint
            ]


frame3d : Frame3d -> Svg Never
frame3d frame =
    let
        originPoint =
            Frame3d.originPoint frame
    in
        Svg.g []
            [ direction3d originPoint (Frame3d.xDirection frame)
            , direction3d originPoint (Frame3d.yDirection frame)
            , direction3d originPoint (Frame3d.zDirection frame)
            , originPoint3d originPoint
            ]


indicator2d : Svg Never
indicator2d =
    Svg.g [ Attributes.stroke "darkgrey" ] [ frame2d Frame2d.xy ]


indicator3d : Svg Never
indicator3d =
    Svg.g [ Attributes.stroke "darkgrey" ] [ frame3d Frame3d.xyz ]


point2d : Point2d -> Svg Never
point2d point =
    let
        place =
            Point2d.placeIn (Frame2d.at point)

        circle =
            Circle2d { centerPoint = point, radius = 2 }

        horizontal =
            LineSegment2d
                ( place (Point2d ( -6, 0 ))
                , place (Point2d ( 6, 0 ))
                )

        vertical =
            LineSegment2d
                ( place (Point2d ( 0, -6 ))
                , place (Point2d ( 0, 6 ))
                )
    in
        Svg.g []
            [ Svg.lineSegment2d [] horizontal
            , Svg.lineSegment2d [] vertical
            , Svg.circle2d [] circle
            ]


point3d : Point3d -> Svg Never
point3d =
    Point3d.projectInto viewPlane >> point2d


axis2d : Axis2d -> Svg Never
axis2d axis =
    let
        originPoint =
            Axis2d.originPoint axis

        segment =
            LineSegment2d ( Point2d.along axis -20, Point2d.along axis 45 )
    in
        Svg.g []
            [ Svg.lineSegment2d
                [ Attributes.strokeDasharray "3 3"
                , Attributes.strokeWidth "0.75"
                ]
                segment
            , originPoint2d originPoint
            , direction2d originPoint (Axis2d.direction axis)
            ]


axis3d : Axis3d -> Svg Never
axis3d axis =
    let
        originPoint =
            Axis3d.originPoint axis

        segment =
            LineSegment3d ( Point3d.along axis -20, Point3d.along axis 45 )
                |> LineSegment3d.projectInto viewPlane
    in
        Svg.g []
            [ Svg.lineSegment2d
                [ Attributes.strokeDasharray "3 3"
                , Attributes.strokeWidth "0.75"
                ]
                segment
            , originPoint3d originPoint
            , direction3d originPoint (Axis3d.direction axis)
            ]


icon2d : Svg Never -> Svg Never
icon2d svg =
    Svg.g [] [ indicator2d, svg ]


icon3d : Svg Never -> Svg Never
icon3d svg =
    Svg.g [] [ indicator3d, svg ]


point2dIcon : Svg Never
point2dIcon =
    icon2d (point2d (Point2d ( 30, 30 )))


point3dIcon : Svg Never
point3dIcon =
    icon3d (point3d (Point3d ( 0, 40, 30 )))


vector2dIcon : Svg Never
vector2dIcon =
    icon2d (vector2d Point2d.origin (Vector2d ( 30, 30 )))


vector3dIcon : Svg Never
vector3dIcon =
    icon3d (vector3d Point3d.origin (Vector3d ( 0, 40, 30 )))


direction2dIcon : Svg Never
direction2dIcon =
    icon2d (direction2d Point2d.origin (Direction2d.fromAngle (degrees 45)))


direction3dIcon : Svg Never
direction3dIcon =
    icon3d (direction3d Point3d.origin (Direction3d ( 0, 0.8, 0.6 )))


axis2dIcon : Svg Never
axis2dIcon =
    let
        axis =
            Axis2d
                { originPoint = Point2d ( 10, 15 )
                , direction = Direction2d.fromAngle (degrees 20)
                }
    in
        icon2d (axis2d axis)


axis3dIcon : Svg Never
axis3dIcon =
    let
        axis =
            Axis3d
                { originPoint = Point3d ( 0, 20, 40 )
                , direction =
                    Direction2d.fromAngle (degrees -20)
                        |> Direction2d.placeOnto SketchPlane3d.yz
                }
    in
        icon3d (axis3d axis)


icons =
    [ point2dIcon
    , point3dIcon
    , vector2dIcon
    , vector3dIcon
    , direction2dIcon
    , direction3dIcon
    , axis2dIcon
    , axis3dIcon
      --, plane3dIcon
      --, sketchPlane3dIcon
      --, frame2dIcon
      --, frame3dIcon
      --, lineSegment2dIcon
      --, lineSegment3dIcon
      --, triangle2dIcon
      --, triangle3dIcon
      --, boundingBox2dIcon
      --, boundingBox3dIcon
      --, circle2dIcon
      --, polyline2dIcon
      --, polyline3dIcon
      --, polygon2dIcon
    ]


topLeftFrame : Frame2d
topLeftFrame =
    Frame2d
        { originPoint = Point2d ( -20, 60 )
        , xDirection = Direction2d.x
        , yDirection = Direction2d.negate Direction2d.y
        }


container : Svg Never -> Html Never
container svg =
    Html.div []
        [ Svg.svg
            [ Attributes.width "80"
            , Attributes.height "80"
            , Attributes.stroke "black"
            ]
            [ Svg.relativeTo topLeftFrame svg
            , Svg.polygon2d
                [ Attributes.strokeWidth "1"
                , Attributes.stroke "lightgrey"
                , Attributes.fill "none"
                ]
                (Polygon2d
                    [ Point2d ( 0, 0 )
                    , Point2d ( 80, 0 )
                    , Point2d ( 80, 80 )
                    , Point2d ( 0, 80 )
                    ]
                )
            ]
        ]


divider : Html Never
divider =
    Html.hr [] []


main : Html Never
main =
    Html.div [] (List.map container icons)
