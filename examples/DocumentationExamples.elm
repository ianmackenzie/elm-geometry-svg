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
    { vector = example ( 90, 90 ) ( 210, 210 ) vectorSvg
    , direction = example ( 90, 90 ) ( 210, 210 ) directionSvg
    , point = example ( 90, 90 ) ( 210, 210 ) pointSvg
    , circle = example ( 130, 130 ) ( 170, 170 ) circleSvg
    , lineSegment = example ( 90, 90 ) ( 210, 210 ) lineSegmentSvg
    , triangle = example ( 90, 90 ) ( 210, 210 ) triangleSvg
    , polyline = example ( 90, 90 ) ( 210, 210 ) polylineSvg
    , polygon = example ( 90, 140 ) ( 210, 210 ) polygonSvg
    , arc = example ( 70, 60 ) ( 170, 170 ) arcSvg
    , quadraticSpline = example ( 35, 35 ) ( 165, 130 ) quadraticSplineSvg
    , scaled = example ( 90, 90 ) ( 250, 250 ) scaledSvg
    , rotated = example ( 130, 80 ) ( 270, 220 ) rotatedSvg
    , translated = example ( 87, 25 ) ( 215, 255 ) translatedSvg
    , mirrored = example ( 35, 20 ) ( 265, 300 ) mirroredSvg
    , placed = example ( 10, 10 ) ( 235, 190 ) placedSvg
    }


type alias Model =
    Maybe (Html Never)


type alias Msg =
    Navigation.Location


main : Program Never Model Msg
main =
    let
        hashParser =
            UrlParser.oneOf
                [ UrlParser.map examples.vector (UrlParser.s "vector")
                , UrlParser.map examples.direction (UrlParser.s "direction")
                , UrlParser.map examples.point (UrlParser.s "point")
                , UrlParser.map examples.circle (UrlParser.s "circle")
                , UrlParser.map examples.lineSegment (UrlParser.s "lineSegment")
                , UrlParser.map examples.triangle (UrlParser.s "triangle")
                , UrlParser.map examples.polyline (UrlParser.s "polyline")
                , UrlParser.map examples.polygon (UrlParser.s "polygon")
                , UrlParser.map examples.arc (UrlParser.s "arc")
                , UrlParser.map examples.quadraticSpline (UrlParser.s "quadraticSpline")
                , UrlParser.map examples.scaled (UrlParser.s "scaled")
                , UrlParser.map examples.rotated (UrlParser.s "rotated")
                , UrlParser.map examples.translated (UrlParser.s "translated")
                , UrlParser.map examples.mirrored (UrlParser.s "mirrored")
                , UrlParser.map examples.placed (UrlParser.s "placed")
                ]

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
                    Html.div []
                        [ Html.text "Please enter an example name:"
                        , Html.ul []
                            [ Html.li [] [ Html.text "vector" ]
                            , Html.li [] [ Html.text "direction" ]
                            , Html.li [] [ Html.text "point" ]
                            , Html.li [] [ Html.text "circle" ]
                            , Html.li [] [ Html.text "lineSegment" ]
                            , Html.li [] [ Html.text "triangle" ]
                            , Html.li [] [ Html.text "polyline" ]
                            , Html.li [] [ Html.text "polygon" ]
                            , Html.li [] [ Html.text "arc" ]
                            , Html.li [] [ Html.text "quadraticSpline" ]
                            , Html.li [] [ Html.text "scaled" ]
                            , Html.li [] [ Html.text "rotated" ]
                            , Html.li [] [ Html.text "translated" ]
                            , Html.li [] [ Html.text "mirrored" ]
                            , Html.li [] [ Html.text "placed" ]
                            ]
                        ]
    in
        Navigation.program identity
            { init = init
            , update = update
            , view = view
            , subscriptions = always Sub.none
            }
