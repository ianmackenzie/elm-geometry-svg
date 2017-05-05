module DocumentationExamples exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
import OpenSolid.CubicSpline2d as CubicSpline2d
import Html exposing (Html)
import Html.Attributes
import Navigation
import UrlParser


vectorSvg : Svg Never
vectorSvg =
    Svg.vector2d
        { tipLength = 30
        , tipWidth = 15
        , tipAttributes =
            [ Attributes.fill "orange"
            , Attributes.stroke "blue"
            , Attributes.strokeWidth "2"
            ]
        , stemAttributes =
            [ Attributes.stroke "blue"
            , Attributes.strokeWidth "3"
            , Attributes.strokeDasharray "3 3"
            ]
        , groupAttributes = []
        }
        (Point2d ( 100, 100 ))
        (Vector2d ( 100, 100 ))


drawDirection : Point2d -> Direction2d -> Svg Never
drawDirection =
    Svg.direction2d
        { length = 100
        , tipLength = 8
        , tipWidth = 8
        , tipAttributes = [ Attributes.fill "orange" ]
        , stemAttributes = []
        , groupAttributes = [ Attributes.stroke "blue" ]
        }


directionSvg : Svg Never
directionSvg =
    let
        basePoint =
            Point2d ( 100, 100 )

        directions =
            [ 0, 15, 30, 45, 60, 75, 90 ]
                |> List.map degrees
                |> List.map Direction2d.fromAngle
    in
        Svg.g [] (List.map (drawDirection basePoint) directions)


drawPoint : Point2d -> Svg Never
drawPoint =
    Svg.point2d
        { radius = 3
        , attributes =
            [ Attributes.stroke "blue"
            , Attributes.fill "orange"
            ]
        }


pointSvg : Svg Never
pointSvg =
    let
        points =
            [ Point2d ( 100, 100 )
            , Point2d ( 200, 200 )
            , Point2d ( 110, 130 )
            , Point2d ( 140, 180 )
            , Point2d ( 170, 110 )
            , Point2d ( 180, 150 )
            , Point2d ( 110, 190 )
            ]
    in
        Svg.g [] (List.map drawPoint points)


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
    let
        spline =
            QuadraticSpline2d
                ( Point2d ( 50, 50 )
                , Point2d ( 100, 150 )
                , Point2d ( 150, 100 )
                )

        ( p1, p2, p3 ) =
            QuadraticSpline2d.controlPoints spline

        points =
            [ p1, p2, p3 ]
    in
        Svg.g [ Attributes.stroke "blue" ]
            [ Svg.quadraticSpline2d
                [ Attributes.strokeWidth "3"
                , Attributes.strokeLinecap "round"
                , Attributes.fill "none"
                ]
                spline
            , Svg.polyline2d
                [ Attributes.strokeWidth "1"
                , Attributes.fill "none"
                , Attributes.strokeDasharray "3 3"
                ]
                (Polyline2d points)
            , Svg.g [ Attributes.fill "white" ]
                (List.map (Svg.point2d { radius = 3, attributes = [] }) points)
            ]


cubicSplineSvg : Svg Never
cubicSplineSvg =
    let
        spline =
            CubicSpline2d
                ( Point2d ( 50, 50 )
                , Point2d ( 100, 150 )
                , Point2d ( 150, 25 )
                , Point2d ( 200, 125 )
                )

        ( p1, p2, p3, p4 ) =
            CubicSpline2d.controlPoints spline

        points =
            [ p1, p2, p3, p4 ]
    in
        Svg.g [ Attributes.stroke "blue" ]
            [ Svg.cubicSpline2d
                [ Attributes.strokeWidth "3"
                , Attributes.strokeLinecap "round"
                , Attributes.fill "none"
                ]
                spline
            , Svg.polyline2d
                [ Attributes.strokeWidth "1"
                , Attributes.fill "none"
                , Attributes.strokeDasharray "3 3"
                ]
                (Polyline2d points)
            , Svg.g [ Attributes.fill "white" ]
                (List.map (Svg.point2d { radius = 3, attributes = [] }) points)
            ]


drawText : Point2d -> String -> String -> String -> Svg Never
drawText point tag anchor baseline =
    Svg.g []
        [ Svg.point2d
            { radius = 2
            , attributes = [ Attributes.fill "orange" ]
            }
            point
        , Svg.text2d
            [ Attributes.textAnchor anchor
            , Attributes.alignmentBaseline baseline
            , Attributes.fill "blue"
            ]
            point
            (tag ++ ": " ++ anchor ++ "/" ++ baseline)
        ]


textSvg : Svg Never
textSvg =
    let
        p1 =
            Point2d ( 100, 100 )

        p2 =
            Point2d ( 300, 145 )

        p3 =
            Point2d ( 175, 190 )

        p4 =
            Point2d ( 300, 250 )
    in
        Svg.g []
            [ drawText p1 "p1" "start" "baseline"
            , drawText p2 "p2" "end" "middle"
                |> Svg.scaleAbout p2 1.33
            , drawText p3 "p3" "middle" "baseline"
                |> Svg.mirrorAcross
                    (Axis2d
                        { originPoint = p3
                        , direction = Direction2d.x
                        }
                    )
            , drawText p4 "p4" "end" "hanging"
                |> Svg.rotateAround p4 (degrees 10)
            ]


scaledSvg : Svg Never
scaledSvg =
    let
        scales =
            [ 1.0, 1.5, 2.25 ]

        referencePoint =
            Point2d ( 100, 100 )

        referencePointSvg =
            Svg.point2d { radius = 3, attributes = [ Attributes.fill "black" ] }
                referencePoint

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
            Svg.point2d { radius = 3, attributes = [ Attributes.fill "black" ] }
                referencePoint

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
example ( minX, minY ) ( maxX, maxY ) =
    Svg.render2d
        (BoundingBox2d
            { minX = minX
            , maxX = maxX
            , minY = minY
            , maxY = maxY
            }
        )


examples =
    [ ( "vector", example ( 90, 90 ) ( 210, 210 ) vectorSvg )
    , ( "direction", example ( 90, 90 ) ( 210, 210 ) directionSvg )
    , ( "point", example ( 90, 90 ) ( 210, 210 ) pointSvg )
    , ( "circle", example ( 130, 130 ) ( 170, 170 ) circleSvg )
    , ( "lineSegment", example ( 90, 90 ) ( 210, 210 ) lineSegmentSvg )
    , ( "triangle", example ( 90, 90 ) ( 210, 210 ) triangleSvg )
    , ( "polyline", example ( 90, 90 ) ( 210, 210 ) polylineSvg )
    , ( "polygon", example ( 90, 140 ) ( 210, 210 ) polygonSvg )
    , ( "arc", example ( 70, 60 ) ( 170, 170 ) arcSvg )
    , ( "quadraticSpline", example ( 35, 35 ) ( 165, 165 ) quadraticSplineSvg )
    , ( "cubicSpline", example ( 30, 5 ) ( 220, 170 ) cubicSplineSvg )
    , ( "text", example ( 90, 90 ) ( 310, 260 ) textSvg )
    , ( "scaled", example ( 90, 90 ) ( 250, 250 ) scaledSvg )
    , ( "rotated", example ( 130, 80 ) ( 270, 220 ) rotatedSvg )
    , ( "translated", example ( 87, 25 ) ( 215, 255 ) translatedSvg )
    , ( "mirrored", example ( 35, 20 ) ( 265, 300 ) mirroredSvg )
    , ( "placed", example ( 10, 10 ) ( 235, 190 ) placedSvg )
    ]


type alias Model =
    Maybe (Html Never)


type alias Msg =
    Navigation.Location


main : Program Never Model Msg
main =
    let
        hashParser =
            UrlParser.oneOf
                (examples
                    |> List.map
                        (\( name, example ) ->
                            UrlParser.s name |> UrlParser.map example
                        )
                )

        parseLocation : Navigation.Location -> Maybe (Html Never)
        parseLocation =
            UrlParser.parseHash hashParser

        init location =
            ( parseLocation location, Cmd.none )

        update location model =
            ( parseLocation location, Cmd.none )

        view model =
            case model of
                Just html ->
                    html |> Html.map never

                Nothing ->
                    let
                        exampleItem ( name, _ ) =
                            Html.li []
                                [ Html.a
                                    [ Html.Attributes.href ("#" ++ name) ]
                                    [ Html.text name ]
                                ]
                    in
                        Html.div []
                            [ Html.text "Please choose an example:"
                            , Html.ul [] (List.map exampleItem examples)
                            ]
    in
        Navigation.program identity
            { init = init
            , update = update
            , view = view
            , subscriptions = always Sub.none
            }
