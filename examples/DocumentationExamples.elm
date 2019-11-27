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

import Angle exposing (Angle)
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
import Parameter1d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Triangle2d exposing (Triangle2d)
import Url exposing (Url)
import Url.Parser exposing ((</>))
import Vector2d exposing (Vector2d)


point2d : List (Svg.Attribute msg) -> Point2d Pixels coordinates -> Svg msg
point2d attributes point =
    point2dWith { radius = Pixels.pixels 3 } attributes point


point2dWith : { radius : Quantity Float Pixels } -> List (Svg.Attribute msg) -> Point2d Pixels coordinates -> Svg msg
point2dWith { radius } attributes point =
    Svg.circle2d attributes (Circle2d.withRadius radius point)


circle : Svg msg
circle =
    Svg.circle2d
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Circle2d.withRadius (Pixels.pixels 10) (Point2d.pixels 150 150))


ellipse : Svg msg
ellipse =
    Svg.ellipse2d
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Ellipse2d.with
            { centerPoint = Point2d.pixels 150 150
            , xDirection = Direction2d.degrees -30
            , xRadius = Pixels.pixels 60
            , yRadius = Pixels.pixels 30
            }
        )


lineSegment : Svg msg
lineSegment =
    Svg.lineSegment2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        ]
        (LineSegment2d.from
            (Point2d.pixels 100 100)
            (Point2d.pixels 200 200)
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
            ( Point2d.pixels 100 100
            , Point2d.pixels 200 100
            , Point2d.pixels 100 200
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
            [ Point2d.pixels 100 100
            , Point2d.pixels 120 200
            , Point2d.pixels 140 100
            , Point2d.pixels 160 200
            , Point2d.pixels 180 100
            , Point2d.pixels 200 200
            ]
        )


polygon : Svg msg
polygon =
    Svg.polygon2d
        [ Attributes.stroke "blue"
        , Attributes.fill "orange"
        , Attributes.strokeWidth "3"
        ]
        (Polygon2d.withHoles
            [ [ Point2d.pixels 150 185
              , Point2d.pixels 165 160
              , Point2d.pixels 135 160
              ]
            ]
            [ Point2d.pixels 100 200
            , Point2d.pixels 120 150
            , Point2d.pixels 180 150
            , Point2d.pixels 200 200
            ]
        )


rectangle : Svg msg
rectangle =
    let
        axes =
            Frame2d.atPoint (Point2d.pixels 150 150)
                |> Frame2d.rotateBy (Angle.degrees 20)
    in
    Svg.rectangle2d
        [ Attributes.stroke "blue"
        , Attributes.fill "orange"
        , Attributes.strokeWidth "4"
        , Attributes.rx "15"
        , Attributes.ry "15"
        ]
        (Rectangle2d.centeredOn axes ( Pixels.pixels 120, Pixels.pixels 80 ))


arc : Svg msg
arc =
    Svg.arc2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        , Attributes.fill "none"
        , Attributes.strokeLinecap "round"
        ]
        (Point2d.pixels 150 75
            |> Arc2d.sweptAround (Point2d.pixels 100 100)
                (Angle.degrees 135)
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
            { centerPoint = Point2d.pixels 100 10
            , xDirection = Direction2d.x
            , xRadius = Pixels.pixels 50
            , yRadius = Pixels.pixels 100
            , startAngle = Angle.degrees 0
            , sweptAngle = Angle.degrees 180
            }
        )


quadraticSpline : Svg msg
quadraticSpline =
    let
        firstControlPoint =
            Point2d.pixels 50 50

        secondControlPoint =
            Point2d.pixels 100 150

        thirdControlPoint =
            Point2d.pixels 150 100

        spline =
            QuadraticSpline2d.fromControlPoints
                firstControlPoint
                secondControlPoint
                thirdControlPoint

        controlPoints =
            [ firstControlPoint, secondControlPoint, thirdControlPoint ]

        drawPoint point =
            Svg.circle2d [] <|
                Circle2d.withRadius (Pixels.pixels 3) point
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
            (Polyline2d.fromVertices controlPoints)
        , Svg.g [ Attributes.fill "white" ]
            (List.map drawPoint controlPoints)
        ]


cubicSpline : Svg msg
cubicSpline =
    let
        firstControlPoint =
            Point2d.pixels 50 50

        secondControlPoint =
            Point2d.pixels 100 150

        thirdControlPoint =
            Point2d.pixels 150 25

        fourthControlPoint =
            Point2d.pixels 200 125

        spline =
            CubicSpline2d.fromControlPoints
                firstControlPoint
                secondControlPoint
                thirdControlPoint
                fourthControlPoint

        controlPoints =
            [ firstControlPoint
            , secondControlPoint
            , thirdControlPoint
            , fourthControlPoint
            ]

        drawPoint point =
            Svg.circle2d [] <|
                Circle2d.withRadius (Pixels.pixels 3) point
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
            (Polyline2d.fromVertices controlPoints)
        , Svg.g [ Attributes.fill "white" ]
            (List.map drawPoint controlPoints)
        ]


scaled : Svg msg
scaled =
    let
        scales =
            [ 1.0, 1.5, 2.25 ]

        referencePoint =
            Point2d.pixels 100 100

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
            Parameter1d.steps 9 <|
                Quantity.interpolateFrom
                    (Angle.degrees 0)
                    (Angle.degrees 270)

        referencePoint =
            Point2d.pixels 200 150

        rotatedCircle : Angle -> Svg msg
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
        , Svg.translateBy (Vector2d.pixels 0 40) polyline
        , Svg.translateBy (Vector2d.pixels 5 -60) polyline
        ]


mirrored : Svg msg
mirrored =
    let
        horizontalAxis =
            Axis2d.through (Point2d.pixels 0 220) Direction2d.x

        horizontalAxisSegment =
            LineSegment2d.along horizontalAxis
                (Pixels.pixels 50)
                (Pixels.pixels 250)

        angledAxis =
            Axis2d.through (Point2d.pixels 0 150)
                (Direction2d.degrees -10)

        angledAxisSegment =
            LineSegment2d.along angledAxis
                (Pixels.pixels 50)
                (Pixels.pixels 250)
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
                    , Point2d.pixels 40 0
                    , Point2d.pixels 50 25
                    , Point2d.pixels 10 25
                    ]
                )

        frames =
            [ Frame2d.atPoint (Point2d.pixels 25 25)
            , Frame2d.atPoint (Point2d.pixels 100 25)
            , Frame2d.atPoint (Point2d.pixels 175 25)
                |> Frame2d.rotateBy (Angle.degrees 20)
            , Frame2d.atPoint (Point2d.pixels 25 150)
            , Frame2d.atPoint (Point2d.pixels 100 100)
                |> Frame2d.rotateBy (Angle.degrees 20)
            , Frame2d.atPoint (Point2d.pixels 150 150)
                |> Frame2d.rotateBy (Angle.degrees -30)
            ]
    in
    Svg.g [] (List.map (\frame -> Svg.placeIn frame stamp) frames)


example : ( Float, Float ) -> ( Float, Float ) -> Svg msg -> Html msg
example ( minX, minY ) ( maxX, maxY ) svg =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels minX maxY)
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
    , ( "rectangle", example ( 50, 75 ) ( 250, 225 ) rectangle )
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
