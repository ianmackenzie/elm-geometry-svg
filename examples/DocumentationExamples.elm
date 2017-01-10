module DocumentationExamples exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import Html exposing (Html)


circleSvg : Svg Never
circleSvg =
    Svg.circle2d
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Circle2d
            { centerPoint = Point2d ( 150, 150 )
            , radius = 10
            }
        )


lineSegmentSvg : Svg Never
lineSegmentSvg =
    Svg.lineSegment2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        ]
        (LineSegment2d
            ( Point2d ( 100, 100 )
            , Point2d ( 200, 200 )
            )
        )


triangleSvg : Svg Never
triangleSvg =
    Svg.triangle2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "orange"
        ]
        (Triangle2d
            ( Point2d ( 100, 100 )
            , Point2d ( 200, 100 )
            , Point2d ( 100, 200 )
            )
        )


polylineSvg : Svg Never
polylineSvg =
    Svg.polyline2d
        [ Attributes.stroke "blue"
        , Attributes.fill "none"
        , Attributes.strokeWidth "5"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        ]
        (Polyline2d
            [ Point2d ( 100, 100 )
            , Point2d ( 120, 200 )
            , Point2d ( 140, 100 )
            , Point2d ( 160, 200 )
            , Point2d ( 180, 100 )
            , Point2d ( 200, 200 )
            ]
        )


polygonSvg : Svg Never
polygonSvg =
    Svg.polygon2d
        [ Attributes.stroke "blue"
        , Attributes.fill "orange"
        , Attributes.strokeWidth "5"
        ]
        (Polygon2d
            [ Point2d ( 100, 200 )
            , Point2d ( 120, 150 )
            , Point2d ( 180, 150 )
            , Point2d ( 200, 200 )
            ]
        )


centerPoint2d : Point2d -> Svg Never
centerPoint2d point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        Svg.g [ Attributes.stroke "black", Attributes.strokeWidth "1" ]
            [ Svg.circle2d [ Attributes.fill "black" ]
                (Circle2d { centerPoint = point, radius = 2 })
            , Svg.lineSegment2d []
                (LineSegment2d ( Point2d ( x - 6, y ), Point2d ( x + 6, y ) ))
            , Svg.lineSegment2d []
                (LineSegment2d ( Point2d ( x, y - 6 ), Point2d ( x, y + 6 ) ))
            ]


scaledSvg : Svg Never
scaledSvg =
    let
        scales =
            [ 1.0, 1.5, 2.25 ]

        referencePoint =
            Point2d ( 100, 100 )

        scaledPoint : Float -> Svg Never
        scaledPoint scale =
            Svg.scaleAbout referencePoint scale circleSvg
    in
        Svg.g []
            (centerPoint2d referencePoint
                :: List.map scaledPoint scales
            )


rotatedSvg : Svg Never
rotatedSvg =
    let
        angles =
            List.range 0 9
                |> List.map (\n -> degrees 30 * toFloat n)

        referencePoint =
            Point2d ( 200, 150 )

        rotatedPoint : Float -> Svg Never
        rotatedPoint angle =
            Svg.rotateAround referencePoint angle circleSvg
    in
        Svg.g []
            (centerPoint2d referencePoint
                :: List.map rotatedPoint angles
            )


translatedSvg : Svg Never
translatedSvg =
    Svg.g []
        [ polylineSvg
        , Svg.translateBy (Vector2d ( 0, 40 )) polylineSvg
        , Svg.translateBy (Vector2d ( 5, -60 )) polylineSvg
        ]


axis2d : Axis2d -> Svg Never
axis2d axis =
    Svg.lineSegment2d
        [ Attributes.strokeWidth "0.5"
        , Attributes.stroke "black"
        , Attributes.strokeDasharray "3 3"
        ]
        (LineSegment2d
            ( Point2d.along axis 50
            , Point2d.along axis 250
            )
        )


mirroredSvg : Svg Never
mirroredSvg =
    let
        horizontalAxis =
            Axis2d
                { originPoint = Point2d ( 0, 220 )
                , direction = Direction2d.x
                }

        angledAxis =
            Axis2d
                { originPoint = Point2d ( 0, 150 )
                , direction = Direction2d.fromAngle (degrees -10)
                }
    in
        Svg.g []
            [ polygonSvg
            , axis2d horizontalAxis
            , Svg.mirrorAcross horizontalAxis polygonSvg
            , axis2d angledAxis
            , Svg.mirrorAcross angledAxis polygonSvg
            ]


direction2d : Point2d -> Direction2d -> Svg msg
direction2d basePoint direction =
    let
        frame =
            Frame2d
                { originPoint = basePoint
                , xDirection = direction
                , yDirection = Direction2d.perpendicularTo direction
                }

        tipPoint =
            Point2d ( 50, 0 )

        stemPoint =
            Point2d ( 45, 0 )

        leftPoint =
            Point2d ( 45, 3 )

        rightPoint =
            Point2d ( 45, -3 )

        stem =
            LineSegment2d ( Point2d.origin, stemPoint )

        tip =
            Triangle2d ( tipPoint, leftPoint, rightPoint )

        attributes =
            [ Attributes.stroke "black"
            , Attributes.fill "white"
            , Attributes.strokeWidth "0.5"
            ]
    in
        Svg.g attributes [ Svg.lineSegment2d [] stem, Svg.triangle2d [] tip ]
            |> Svg.placeIn frame


frame2d : Frame2d -> Svg msg
frame2d frame =
    let
        originPoint =
            Frame2d.originPoint frame

        xDirection =
            Frame2d.xDirection frame

        yDirection =
            Frame2d.yDirection frame
    in
        Svg.g []
            [ direction2d originPoint xDirection
            , direction2d originPoint yDirection
            , Svg.circle2d
                [ Attributes.stroke "black"
                , Attributes.strokeWidth "0.5"
                , Attributes.fill "white"
                ]
                (Circle2d { centerPoint = originPoint, radius = 3 })
            ]


placedSvg : Svg Never
placedSvg =
    let
        stampSvg =
            Svg.polygon2d
                [ Attributes.fill "orange"
                , Attributes.stroke "blue"
                , Attributes.strokeWidth "2"
                ]
                (Polygon2d
                    [ Point2d.origin
                    , Point2d ( 40, 0 )
                    , Point2d ( 50, 25 )
                    , Point2d ( 10, 25 )
                    ]
                )

        frames =
            [ Frame2d.at (Point2d ( 25, 25 ))
            , Frame2d.at (Point2d ( 100, 25 ))
            , Frame2d.at (Point2d ( 175, 25 ))
                |> Frame2d.rotateBy (degrees 20)
            , Frame2d.at (Point2d ( 25, 150 ))
            , Frame2d.at (Point2d ( 100, 100 ))
                |> Frame2d.rotateBy (degrees 20)
            , Frame2d.at (Point2d ( 150, 150 ))
                |> Frame2d.rotateBy (degrees -30)
            ]
    in
        Svg.g [] (List.map (\frame -> Svg.placeIn frame stampSvg) frames)


example : ( Float, Float ) -> ( Float, Float ) -> Svg Never -> Html Never
example ( minX, minY ) ( maxX, maxY ) svg =
    let
        width =
            maxX - minX

        height =
            maxY - minY

        topLeftFrame =
            Frame2d
                { originPoint = Point2d ( minX, maxY )
                , xDirection = Direction2d.x
                , yDirection = Direction2d.negate Direction2d.y
                }

        outline =
            Polygon2d
                [ Point2d.origin
                , Point2d ( 0, height )
                , Point2d ( width, height )
                , Point2d ( width, 0 )
                ]
    in
        Html.div []
            [ Svg.svg
                [ Attributes.width (toString width)
                , Attributes.height (toString height)
                ]
                [ Svg.relativeTo topLeftFrame svg ]
            ]


examples =
    [ example ( 130, 130 ) ( 170, 170 ) circleSvg
    , example ( 90, 90 ) ( 210, 210 ) lineSegmentSvg
    , example ( 90, 90 ) ( 210, 210 ) triangleSvg
    , example ( 90, 90 ) ( 210, 210 ) polylineSvg
    , example ( 90, 140 ) ( 210, 210 ) polygonSvg
    , example ( 90, 90 ) ( 250, 250 ) scaledSvg
    , example ( 130, 80 ) ( 270, 220 ) rotatedSvg
    , example ( 87, 25 ) ( 215, 255 ) translatedSvg
    , example ( 35, 20 ) ( 265, 300 ) mirroredSvg
    , example ( 10, 10 ) ( 235, 190 ) placedSvg
    ]


divider : Html Never
divider =
    Html.hr [] []


main : Html Never
main =
    Html.div [] (examples |> List.intersperse divider)
