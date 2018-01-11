module DocumentationExamples exposing (..)

import Html exposing (Html)
import Html.Attributes
import Navigation
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Ellipse2d as Ellipse2d exposing (Ellipse2d)
import OpenSolid.EllipticalArc2d as EllipticalArc2d exposing (EllipticalArc2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.Svg as Svg
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import UrlParser


vector : Svg Never
vector =
    Svg.vector2d
        [ Attributes.fill "black"
        , Attributes.stroke "black"
        ]
        (Point2d.fromCoordinates ( 100, 100 ))
        (Vector2d.fromComponents ( 100, 100 ))


customVector : Svg Never
customVector =
    Svg.vector2dWith { tipLength = 30, tipWidth = 30 }
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Point2d.fromCoordinates ( 100, 100 ))
        (Vector2d.fromComponents ( 100, 100 ))


directions : Svg Never
directions =
    let
        basePoint =
            Point2d.fromCoordinates ( 100, 100 )

        angles =
            List.map degrees [ 5, 25, 45, 65, 85 ]

        values =
            List.map Direction2d.fromAngle angles

        attributes =
            [ Attributes.fill "white"
            , Attributes.stroke "black"
            ]
    in
    Svg.g attributes <|
        List.map (Svg.direction2d [] basePoint) values


customDirections : Svg Never
customDirections =
    let
        basePoint =
            Point2d.fromCoordinates ( 100, 100 )

        angles =
            List.map degrees [ 5, 25, 45, 65, 85 ]

        values =
            List.map Direction2d.fromAngle angles

        attributes =
            [ Attributes.fill "orange"
            , Attributes.stroke "blue"
            , Attributes.strokeWidth "2"
            ]

        drawDirection =
            Svg.direction2dWith
                { length = 100
                , tipLength = 20
                , tipWidth = 20
                }
                []
                basePoint
    in
    Svg.g attributes (List.map drawDirection values)


points : Svg Never
points =
    let
        values =
            [ Point2d.fromCoordinates ( 110, 130 )
            , Point2d.fromCoordinates ( 140, 180 )
            , Point2d.fromCoordinates ( 170, 110 )
            , Point2d.fromCoordinates ( 180, 150 )
            ]

        attributes =
            [ Attributes.stroke "black"
            , Attributes.fill "white"
            ]
    in
    Svg.g attributes (List.map (Svg.point2d []) values)


customPoints : Svg Never
customPoints =
    let
        values =
            [ Point2d.fromCoordinates ( 110, 130 )
            , Point2d.fromCoordinates ( 140, 180 )
            , Point2d.fromCoordinates ( 170, 110 )
            , Point2d.fromCoordinates ( 180, 150 )
            ]

        attributes =
            [ Attributes.stroke "blue"
            , Attributes.fill "orange"
            , Attributes.strokeWidth "2"
            ]

        drawPoint =
            Svg.point2dWith { radius = 6 } []
    in
    Svg.g attributes (List.map drawPoint values)


circle : Svg Never
circle =
    Svg.circle2d
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Circle2d.with
            { centerPoint = Point2d.fromCoordinates ( 150, 150 )
            , radius = 10
            }
        )


ellipse : Svg Never
ellipse =
    Svg.ellipse2d
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Ellipse2d.with
            { centerPoint =
                Point2d.fromCoordinates ( 150, 150 )
            , xDirection =
                Direction2d.fromAngle (degrees -30)
            , xRadius = 60
            , yRadius = 30
            }
        )


lineSegment : Svg Never
lineSegment =
    Svg.lineSegment2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates ( 100, 100 )
            , Point2d.fromCoordinates ( 200, 200 )
            )
        )


triangle : Svg Never
triangle =
    Svg.triangle2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "orange"
        ]
        (Triangle2d.fromVertices
            ( Point2d.fromCoordinates ( 100, 100 )
            , Point2d.fromCoordinates ( 200, 100 )
            , Point2d.fromCoordinates ( 100, 200 )
            )
        )


polyline : Svg Never
polyline =
    Svg.polyline2d
        [ Attributes.stroke "blue"
        , Attributes.fill "none"
        , Attributes.strokeWidth "5"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        ]
        (Polyline2d.fromVertices
            [ Point2d.fromCoordinates ( 100, 100 )
            , Point2d.fromCoordinates ( 120, 200 )
            , Point2d.fromCoordinates ( 140, 100 )
            , Point2d.fromCoordinates ( 160, 200 )
            , Point2d.fromCoordinates ( 180, 100 )
            , Point2d.fromCoordinates ( 200, 200 )
            ]
        )


polygon : Svg Never
polygon =
    Svg.polygon2d
        [ Attributes.stroke "blue"
        , Attributes.fill "orange"
        , Attributes.strokeWidth "5"
        ]
        (Polygon2d.fromVertices
            [ Point2d.fromCoordinates ( 100, 200 )
            , Point2d.fromCoordinates ( 120, 150 )
            , Point2d.fromCoordinates ( 180, 150 )
            , Point2d.fromCoordinates ( 200, 200 )
            ]
        )


arc : Svg Never
arc =
    Svg.arc2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        , Attributes.fill "none"
        , Attributes.strokeLinecap "round"
        ]
        (Arc2d.with
            { centerPoint = Point2d.fromCoordinates ( 100, 100 )
            , startPoint = Point2d.fromCoordinates ( 150, 75 )
            , sweptAngle = degrees 135
            }
        )


ellipticalArc : Svg Never
ellipticalArc =
    Svg.ellipticalArc2d
        [ Attributes.stroke "blue"
        , Attributes.fill "none"
        , Attributes.strokeWidth "5"
        , Attributes.strokeLinecap "round"
        ]
        (EllipticalArc2d.with
            { centerPoint =
                Point2d.fromCoordinates ( 100, 10 )
            , xDirection = Direction2d.x
            , xRadius = 50
            , yRadius = 100
            , startAngle = 0
            , sweptAngle = degrees 180
            }
        )


quadraticSpline : Svg Never
quadraticSpline =
    let
        spline =
            QuadraticSpline2d.fromControlPoints
                ( Point2d.fromCoordinates ( 50, 50 )
                , Point2d.fromCoordinates ( 100, 150 )
                , Point2d.fromCoordinates ( 150, 100 )
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
            (Polyline2d.fromVertices points)
        , Svg.g [ Attributes.fill "white" ] (List.map (Svg.point2d []) points)
        ]


cubicSpline : Svg Never
cubicSpline =
    let
        spline =
            CubicSpline2d.fromControlPoints
                ( Point2d.fromCoordinates ( 50, 50 )
                , Point2d.fromCoordinates ( 100, 150 )
                , Point2d.fromCoordinates ( 150, 25 )
                , Point2d.fromCoordinates ( 200, 125 )
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
            (Polyline2d.fromVertices points)
        , Svg.g [ Attributes.fill "white" ] (List.map (Svg.point2d []) points)
        ]


drawText : Point2d -> String -> String -> Svg Never
drawText point anchor baseline =
    Svg.g []
        [ Svg.point2dWith { radius = 2 } [ Attributes.fill "orange" ] point
        , Svg.text2d
            [ Attributes.textAnchor anchor
            , Attributes.dominantBaseline baseline
            , Attributes.fill "blue"
            ]
            point
            (anchor ++ "/" ++ baseline)
        ]


text : Svg Never
text =
    let
        p1 =
            Point2d.fromCoordinates ( 100, 100 )

        p2 =
            Point2d.fromCoordinates ( 300, 145 )

        p3 =
            Point2d.fromCoordinates ( 175, 190 )

        p4 =
            Point2d.fromCoordinates ( 300, 250 )
    in
    Svg.g []
        [ drawText p1 "start" "baseline"
        , drawText p2 "end" "middle"
            |> Svg.scaleAbout p2 1.33
        , drawText p3 "middle" "baseline"
            |> Svg.mirrorAcross
                (Axis2d.with
                    { originPoint = p3
                    , direction = Direction2d.x
                    }
                )
        , drawText p4 "end" "hanging"
            |> Svg.rotateAround p4 (degrees 10)
        ]


scaled : Svg Never
scaled =
    let
        scales =
            [ 1.0, 1.5, 2.25 ]

        referencePoint =
            Point2d.fromCoordinates ( 100, 100 )

        scaledCircle : Float -> Svg Never
        scaledCircle scale =
            Svg.scaleAbout referencePoint scale circle
    in
    Svg.g []
        (Svg.point2d [ Attributes.fill "black" ] referencePoint
            :: List.map scaledCircle scales
        )


rotated : Svg Never
rotated =
    let
        angles =
            List.range 0 9
                |> List.map (\n -> degrees 30 * toFloat n)

        referencePoint =
            Point2d.fromCoordinates ( 200, 150 )

        rotatedCircle : Float -> Svg Never
        rotatedCircle angle =
            Svg.rotateAround referencePoint angle circle
    in
    Svg.g []
        (Svg.point2d [ Attributes.fill "black" ] referencePoint
            :: List.map rotatedCircle angles
        )


translated : Svg Never
translated =
    Svg.g []
        [ polyline
        , Svg.translateBy (Vector2d.fromComponents ( 0, 40 )) polyline
        , Svg.translateBy (Vector2d.fromComponents ( 5, -60 )) polyline
        ]


mirrored : Svg Never
mirrored =
    let
        horizontalAxis =
            Axis2d.with
                { originPoint = Point2d.fromCoordinates ( 0, 220 )
                , direction = Direction2d.x
                }

        horizontalAxisSegment =
            LineSegment2d.along horizontalAxis 50 250

        angledAxis =
            Axis2d.with
                { originPoint = Point2d.fromCoordinates ( 0, 150 )
                , direction = Direction2d.fromAngle (degrees -10)
                }

        angledAxisSegment =
            LineSegment2d.along angledAxis 50 250
    in
    Svg.g []
        [ polygon
        , Svg.mirrorAcross horizontalAxis polygon
        , Svg.mirrorAcross angledAxis polygon
        , Svg.g
            [ Attributes.strokeWidth "0.5"
            , Attributes.stroke "black"
            , Attributes.strokeDasharray "3 3"
            ]
            [ Svg.lineSegment2d [] horizontalAxisSegment
            , Svg.lineSegment2d [] angledAxisSegment
            ]
        ]


placed : Svg Never
placed =
    let
        stamp =
            Svg.polygon2d
                [ Attributes.fill "orange"
                , Attributes.stroke "blue"
                , Attributes.strokeWidth "2"
                ]
                (Polygon2d.fromVertices
                    [ Point2d.origin
                    , Point2d.fromCoordinates ( 40, 0 )
                    , Point2d.fromCoordinates ( 50, 25 )
                    , Point2d.fromCoordinates ( 10, 25 )
                    ]
                )

        frames =
            [ Frame2d.atPoint (Point2d.fromCoordinates ( 25, 25 ))
            , Frame2d.atPoint (Point2d.fromCoordinates ( 100, 25 ))
            , Frame2d.atPoint (Point2d.fromCoordinates ( 175, 25 ))
                |> Frame2d.rotateBy (degrees 20)
            , Frame2d.atPoint (Point2d.fromCoordinates ( 25, 150 ))
            , Frame2d.atPoint (Point2d.fromCoordinates ( 100, 100 ))
                |> Frame2d.rotateBy (degrees 20)
            , Frame2d.atPoint (Point2d.fromCoordinates ( 150, 150 ))
                |> Frame2d.rotateBy (degrees -30)
            ]
    in
    Svg.g [] (List.map (\frame -> Svg.placeIn frame stamp) frames)


example : ( Float, Float ) -> ( Float, Float ) -> Svg Never -> Html Never
example ( minX, minY ) ( maxX, maxY ) =
    Svg.render2d
        (BoundingBox2d.with
            { minX = minX
            , maxX = maxX
            , minY = minY
            , maxY = maxY
            }
        )


examples : List ( String, Html Never )
examples =
    [ ( "vector", example ( 90, 90 ) ( 210, 210 ) vector )
    , ( "customVector", example ( 90, 90 ) ( 210, 210 ) customVector )
    , ( "directions", example ( 90, 90 ) ( 160, 160 ) directions )
    , ( "customDirections", example ( 90, 90 ) ( 210, 210 ) customDirections )
    , ( "points", example ( 90, 90 ) ( 200, 200 ) points )
    , ( "customPoints", example ( 90, 90 ) ( 200, 200 ) customPoints )
    , ( "circle", example ( 130, 130 ) ( 170, 170 ) circle )
    , ( "ellipse", example ( 80, 100 ) ( 220, 200 ) ellipse )
    , ( "lineSegment", example ( 90, 90 ) ( 210, 210 ) lineSegment )
    , ( "triangle", example ( 90, 90 ) ( 210, 210 ) triangle )
    , ( "polyline", example ( 90, 90 ) ( 210, 210 ) polyline )
    , ( "polygon", example ( 90, 140 ) ( 210, 210 ) polygon )
    , ( "arc", example ( 70, 60 ) ( 170, 170 ) arc )
    , ( "ellipticalArc", example ( 40, 0 ) ( 160, 120 ) ellipticalArc )
    , ( "quadraticSpline", example ( 35, 35 ) ( 165, 165 ) quadraticSpline )
    , ( "cubicSpline", example ( 30, 5 ) ( 220, 170 ) cubicSpline )
    , ( "text", example ( 90, 90 ) ( 310, 260 ) text )
    , ( "scaled", example ( 90, 90 ) ( 250, 250 ) scaled )
    , ( "rotated", example ( 130, 80 ) ( 270, 220 ) rotated )
    , ( "translated", example ( 87, 25 ) ( 215, 255 ) translated )
    , ( "mirrored", example ( 35, 20 ) ( 265, 300 ) mirrored )
    , ( "placed", example ( 10, 10 ) ( 235, 190 ) placed )
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
