module DocumentationExamples exposing
    ( Model
    , Msg
    , arc
    , circle
    , cubicSpline
    , ellipse
    , ellipticalArc
    , example
    , examples
    , lineSegment
    , main
    , mirrored
    , placed
    , point2d
    , point2dWith
    , polygon
    , polyline
    , quadraticSpline
    , rotated
    , scaled
    , translated
    , triangle
    )

import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import Browser
import Browser.Navigation as Navigation
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import Dict
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Triangle2d exposing (Triangle2d)
import Url exposing (Url)
import Url.Parser exposing ((</>))
import Vector2d exposing (Vector2d)


point2d : List (Svg.Attribute msg) -> Point2d -> Svg msg
point2d attributes point =
    point2dWith { radius = 3 } attributes point


point2dWith : { radius : Float } -> List (Svg.Attribute msg) -> Point2d -> Svg msg
point2dWith { radius } attributes point =
    Svg.circle2d attributes (Circle2d.withRadius radius point)


circle : Svg msg
circle =
    Svg.circle2d
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Circle2d.withRadius 10 (Point2d.fromCoordinates ( 150, 150 )))


ellipse : Svg msg
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


lineSegment : Svg msg
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


triangle : Svg msg
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


polyline : Svg msg
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


polygon : Svg msg
polygon =
    Svg.polygon2d
        [ Attributes.stroke "blue"
        , Attributes.fill "orange"
        , Attributes.strokeWidth "3"
        ]
        (Polygon2d.with
            { outerLoop =
                [ Point2d.fromCoordinates ( 100, 200 )
                , Point2d.fromCoordinates ( 120, 150 )
                , Point2d.fromCoordinates ( 180, 150 )
                , Point2d.fromCoordinates ( 200, 200 )
                ]
            , innerLoops =
                [ [ Point2d.fromCoordinates ( 150, 185 )
                  , Point2d.fromCoordinates ( 165, 160 )
                  , Point2d.fromCoordinates ( 135, 160 )
                  ]
                ]
            }
        )


arc : Svg msg
arc =
    Svg.arc2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        , Attributes.fill "none"
        , Attributes.strokeLinecap "round"
        ]
        (Point2d.fromCoordinates ( 150, 75 )
            |> Arc2d.sweptAround (Point2d.fromCoordinates ( 100, 100 ))
                (degrees 135)
        )


ellipticalArc : Svg msg
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


quadraticSpline : Svg msg
quadraticSpline =
    let
        startPoint =
            Point2d.fromCoordinates ( 50, 50 )

        controlPoint =
            Point2d.fromCoordinates ( 100, 150 )

        endPoint =
            Point2d.fromCoordinates ( 150, 100 )

        spline =
            QuadraticSpline2d.with
                { startPoint = startPoint
                , controlPoint = controlPoint
                , endPoint = endPoint
                }

        points =
            [ startPoint, controlPoint, endPoint ]

        drawPoint point =
            Svg.circle2d []
                (Circle2d.withRadius 3 point)
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
        , Svg.g [ Attributes.fill "white" ]
            (List.map drawPoint points)
        ]


cubicSpline : Svg msg
cubicSpline =
    let
        startPoint =
            Point2d.fromCoordinates ( 50, 50 )

        startControlPoint =
            Point2d.fromCoordinates ( 100, 150 )

        endControlPoint =
            Point2d.fromCoordinates ( 150, 25 )

        endPoint =
            Point2d.fromCoordinates ( 200, 125 )

        spline =
            CubicSpline2d.with
                { startPoint = startPoint
                , startControlPoint = startControlPoint
                , endControlPoint = endControlPoint
                , endPoint = endPoint
                }

        points =
            [ startPoint
            , startControlPoint
            , endControlPoint
            , endPoint
            ]

        drawPoint point =
            Svg.circle2d []
                (Circle2d.withRadius 3 point)
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
        , Svg.g [ Attributes.fill "white" ]
            (List.map drawPoint points)
        ]


scaled : Svg msg
scaled =
    let
        scales =
            [ 1.0, 1.5, 2.25 ]

        referencePoint =
            Point2d.fromCoordinates ( 100, 100 )

        scaledCircle : Float -> Svg msg
        scaledCircle scale =
            Svg.scaleAbout referencePoint scale circle
    in
    Svg.g []
        (point2d [ Attributes.fill "black" ] referencePoint
            :: List.map scaledCircle scales
        )


rotated : Svg msg
rotated =
    let
        angles =
            List.range 0 9
                |> List.map (\n -> degrees 30 * toFloat n)

        referencePoint =
            Point2d.fromCoordinates ( 200, 150 )

        rotatedCircle : Float -> Svg msg
        rotatedCircle angle =
            Svg.rotateAround referencePoint angle circle
    in
    Svg.g []
        (point2d [ Attributes.fill "black" ] referencePoint
            :: List.map rotatedCircle angles
        )


translated : Svg msg
translated =
    Svg.g []
        [ polyline
        , Svg.translateBy (Vector2d.fromComponents ( 0, 40 )) polyline
        , Svg.translateBy (Vector2d.fromComponents ( 5, -60 )) polyline
        ]


mirrored : Svg msg
mirrored =
    let
        horizontalAxis =
            Axis2d.through (Point2d.fromCoordinates ( 0, 220 )) Direction2d.x

        horizontalAxisSegment =
            LineSegment2d.along horizontalAxis 50 250

        angledAxis =
            Axis2d.through (Point2d.fromCoordinates ( 0, 150 ))
                (Direction2d.fromAngle (degrees -10))

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


placed : Svg msg
placed =
    let
        stamp =
            Svg.polygon2d
                [ Attributes.fill "orange"
                , Attributes.stroke "blue"
                , Attributes.strokeWidth "2"
                ]
                (Polygon2d.singleLoop
                    [ Point2d.origin
                    , Point2d.fromCoordinates ( 40, 0 )
                    , Point2d.fromCoordinates ( 50, 25 )
                    , Point2d.fromCoordinates ( 10, 25 )
                    ]
                )

        frames =
            [ Frame2d.atCoordinates ( 25, 25 )
            , Frame2d.atCoordinates ( 100, 25 )
            , Frame2d.atCoordinates ( 175, 25 )
                |> Frame2d.rotateBy (degrees 20)
            , Frame2d.atCoordinates ( 25, 150 )
            , Frame2d.atCoordinates ( 100, 100 )
                |> Frame2d.rotateBy (degrees 20)
            , Frame2d.atCoordinates ( 150, 150 )
                |> Frame2d.rotateBy (degrees -30)
            ]
    in
    Svg.g [] (List.map (\frame -> Svg.placeIn frame stamp) frames)


example : ( Float, Float ) -> ( Float, Float ) -> Svg msg -> Html msg
example ( minX, minY ) ( maxX, maxY ) svg =
    let
        topLeftFrame =
            Frame2d.atCoordinates ( minX, maxY )
                |> Frame2d.reverseY

        width =
            maxX - minX

        height =
            maxY - minY
    in
    Svg.svg
        [ Attributes.width (String.fromFloat width)
        , Attributes.height (String.fromFloat height)
        , Html.Attributes.style "display" "block"
        ]
        [ svg |> Svg.relativeTo topLeftFrame ]


examples : List ( String, Html Never )
examples =
    [ ( "circle", example ( 130, 130 ) ( 170, 170 ) circle )
    , ( "ellipse", example ( 80, 100 ) ( 220, 200 ) ellipse )
    , ( "lineSegment", example ( 90, 90 ) ( 210, 210 ) lineSegment )
    , ( "triangle", example ( 90, 90 ) ( 210, 210 ) triangle )
    , ( "polyline", example ( 90, 90 ) ( 210, 210 ) polyline )
    , ( "polygon", example ( 90, 140 ) ( 210, 210 ) polygon )
    , ( "arc", example ( 70, 60 ) ( 170, 170 ) arc )
    , ( "ellipticalArc", example ( 40, 0 ) ( 160, 120 ) ellipticalArc )
    , ( "quadraticSpline", example ( 35, 35 ) ( 165, 165 ) quadraticSpline )
    , ( "cubicSpline", example ( 30, 5 ) ( 220, 170 ) cubicSpline )
    , ( "scaled", example ( 90, 90 ) ( 250, 250 ) scaled )
    , ( "rotated", example ( 130, 80 ) ( 270, 220 ) rotated )
    , ( "translated", example ( 87, 25 ) ( 215, 255 ) translated )
    , ( "mirrored", example ( 35, 20 ) ( 265, 300 ) mirrored )
    , ( "placed", example ( 10, 10 ) ( 235, 190 ) placed )
    ]


type alias Model =
    { displayedExample : Maybe (Html Never)
    , navigationKey : Navigation.Key
    }


type Msg
    = UrlChange Url
    | UrlRequest Browser.UrlRequest


main : Program () Model Msg
main =
    let
        examplesByName =
            Dict.fromList examples

        getExample : Maybe String -> Maybe (Html Never)
        getExample fragment =
            fragment |> Maybe.andThen (\name -> Dict.get name examplesByName)

        hashParser : Url.Parser.Parser (Maybe (Html Never) -> a) a
        hashParser =
            Url.Parser.s "DocumentationExamples.elm"
                </> Url.Parser.fragment getExample

        parse : Url -> Maybe (Html Never)
        parse url =
            case Url.Parser.parse hashParser url of
                Nothing ->
                    Nothing

                Just parseResult ->
                    parseResult

        init flags url key =
            ( { displayedExample = parse url, navigationKey = key }
            , Cmd.none
            )

        update message model =
            case message of
                UrlChange url ->
                    ( { model | displayedExample = parse url }, Cmd.none )

                UrlRequest (Browser.Internal url) ->
                    ( model
                    , Navigation.pushUrl model.navigationKey (Url.toString url)
                    )

                UrlRequest (Browser.External url) ->
                    ( model
                    , Navigation.load url
                    )

        view model =
            { title = "DocumentationExamples"
            , body =
                [ case model.displayedExample of
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
                ]
            }
    in
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
