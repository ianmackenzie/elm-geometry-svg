module DocumentationExamples exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.LineSegment2d as LineSegment2d
import Html exposing (Html)
import Html.Attributes


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


arcSvg : Svg Never
arcSvg =
    Svg.arc2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        , Attributes.fill "none"
        , Attributes.strokeLinecap "round"
        ]
        (Arc2d
            { centerPoint = Point2d ( 100, 100 )
            , startPoint = Point2d ( 150, 75 )
            , sweptAngle = degrees 135
            }
        )


quadraticSplineSvg : Svg Never
quadraticSplineSvg =
    Svg.quadraticSpline2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "3"
        , Attributes.strokeLinecap "round"
        , Attributes.fill "none"
        ]
        (QuadraticSpline2d
            ( Point2d ( 50, 50 )
            , Point2d ( 100, 150 )
            , Point2d ( 150, 100 )
            )
        )


scaledSvg : Svg Never
scaledSvg =
    let
        scales =
            [ 1.0, 1.5, 2.25 ]

        referencePoint =
            Point2d ( 100, 100 )

        referencePointSvg =
            Svg.circle2d [ Attributes.fill "black" ]
                (Circle2d { centerPoint = referencePoint, radius = 3 })

        scaledCircle : Float -> Svg Never
        scaledCircle scale =
            Svg.scaleAbout referencePoint scale circleSvg
    in
        Svg.g [] (referencePointSvg :: List.map scaledCircle scales)


rotatedSvg : Svg Never
rotatedSvg =
    let
        angles =
            List.range 0 9
                |> List.map (\n -> degrees 30 * toFloat n)

        referencePoint =
            Point2d ( 200, 150 )

        referencePointSvg =
            Svg.circle2d [ Attributes.fill "black" ]
                (Circle2d { centerPoint = referencePoint, radius = 3 })

        rotatedCircle : Float -> Svg Never
        rotatedCircle angle =
            Svg.rotateAround referencePoint angle circleSvg
    in
        Svg.g [] (referencePointSvg :: List.map rotatedCircle angles)


translatedSvg : Svg Never
translatedSvg =
    Svg.g []
        [ polylineSvg
        , Svg.translateBy (Vector2d ( 0, 40 )) polylineSvg
        , Svg.translateBy (Vector2d ( 5, -60 )) polylineSvg
        ]


mirroredSvg : Svg Never
mirroredSvg =
    let
        horizontalAxis =
            Axis2d
                { originPoint = Point2d ( 0, 220 )
                , direction = Direction2d.x
                }

        horizontalAxisSegment =
            LineSegment2d.along horizontalAxis 50 250

        angledAxis =
            Axis2d
                { originPoint = Point2d ( 0, 150 )
                , direction = Direction2d.fromAngle (degrees -10)
                }

        angledAxisSegment =
            LineSegment2d.along angledAxis 50 250
    in
        Svg.g []
            [ polygonSvg
            , Svg.mirrorAcross horizontalAxis polygonSvg
            , Svg.mirrorAcross angledAxis polygonSvg
            , Svg.g
                [ Attributes.strokeWidth "0.5"
                , Attributes.stroke "black"
                , Attributes.strokeDasharray "3 3"
                ]
                [ Svg.lineSegment2d [] horizontalAxisSegment
                , Svg.lineSegment2d [] angledAxisSegment
                ]
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
                , xDirection = Direction2d.positiveX
                , yDirection = Direction2d.negativeY
                }
    in
        Html.div
            [ Html.Attributes.style
                [ ( "border", "1px solid lightgrey" )
                , ( "margin", "10px" )
                ]
            ]
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
    , example ( 70, 60 ) ( 170, 170 ) arcSvg
    , example ( 35, 35 ) ( 165, 130 ) quadraticSplineSvg
    , example ( 90, 90 ) ( 250, 250 ) scaledSvg
    , example ( 130, 80 ) ( 270, 220 ) rotatedSvg
    , example ( 87, 25 ) ( 215, 255 ) translatedSvg
    , example ( 35, 20 ) ( 265, 300 ) mirroredSvg
    , example ( 10, 10 ) ( 235, 190 ) placedSvg
    ]


main : Html Never
main =
    Html.div [] examples
