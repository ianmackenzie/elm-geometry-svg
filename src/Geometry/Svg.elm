module Geometry.Svg exposing
    ( lineSegment2d, triangle2d, polyline2d, polygon2d, arc2d, ellipticalArc2d, circle2d, ellipse2d, quadraticSpline2d, cubicSpline2d, boundingBox2d
    , scaleAbout, rotateAround, translateBy, mirrorAcross
    , relativeTo, placeIn
    )

{-| Draw 2D `elm-geometry` values as SVG.


## Reading this documentation

For the examples, assume that the following imports are present:

    import Svg exposing (Svg)
    import Svg.Attributes as Attributes
    import Geometry.Svg as Svg

Also assume that any necessary `elm-geometry` modules/types have been imported
using the following format:

    import Point2d exposing (Point2d)

All examples use a Y-up coordinate system instead of SVG's Y-down (window)
coordinate system; they were all rendered with a final [`relativeTo`](#relativeTo)
call to flip the example 'upside down' for display.


# Geometry

These functions turn `elm-geometry` 2D values into SVG elements with geometric
attributes such as `points` and `transform` set appropriately. Each function
also accepts a list of additional SVG attributes such as `fill` or `stroke` that
should be added to the resulting element.

@docs lineSegment2d, triangle2d, polyline2d, polygon2d, arc2d, ellipticalArc2d, circle2d, ellipse2d, quadraticSpline2d, cubicSpline2d, boundingBox2d


# Transformations

These functions allow you to use all the normal `elm-geometry` 2D transformations on
arbitrary fragments of SVG. For example,

    Svg.mirrorAcross Axis2d.x
        (Svg.lineSegment2d [] lineSegment)

draws a line segment as SVG and then mirrors that SVG fragment. This is visually
the same as

    Svg.lineSegment2d []
        (LineSegment2d.mirrorAcross Axis2d.x lineSegment)

which instead mirrors the line segment first and then draws the mirrored line
segment as SVG.

In the above example only a single SVG element was transformed, but all of these
transformation functions work equally well on arbitrarily complex fragments of
SVG such as nested groups of elements of different types:

    Svg.rotateAround Point2d.origin
        (degrees 30)
        (Svg.g [ Attributes.stroke "blue" ]
            [ Svg.lineSegment2d [] lineSegment
            , Svg.circle2d [] someCircle
            , Svg.g [ Attributes.fill "orange" ]
                [ Svg.triangle2d [] firstTriangle
                , Svg.triangle2d [] secondTriangle
                ]
            ]
        )

If the transformation changes frequently (an animated rotation angle, for
example) while the geometry itself remains constant, using an SVG transformation
can be more efficient since the geometry itself does not have to be recreated
(the SVG virtual DOM only has to update a transformation matrix).

@docs scaleAbout, rotateAround, translateBy, mirrorAcross


# Coordinate transformations

Similar to the above transformations, these functions allow `elm-geometry`
coordinate conversion transformations to be applied to arbitrary SVG elements.

@docs relativeTo, placeIn

-}

import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import Curve.ParameterValue as ParameterValue exposing (ParameterValue)
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attributes
import Triangle2d exposing (Triangle2d)
import Vector2d exposing (Vector2d)


inDegrees : Float -> Float
inDegrees angle =
    angle / degrees 1


coordinatesString : Point2d -> String
coordinatesString point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    String.fromFloat x ++ "," ++ String.fromFloat y


pointsAttribute : List Point2d -> Attribute msg
pointsAttribute points =
    Attributes.points (String.join " " (List.map coordinatesString points))


{-| Draw a `LineSegment2d` as an SVG `<polyline>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#lineSegment" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#lineSegment`
</iframe>

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

-}
lineSegment2d : List (Attribute msg) -> LineSegment2d -> Svg msg
lineSegment2d attributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment
    in
    Svg.polyline (pointsAttribute [ p1, p2 ] :: attributes) []


{-| Draw a `Triangle2d` as an SVG `<polygon>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#triangle" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#triangle`
</iframe>

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

-}
triangle2d : List (Attribute msg) -> Triangle2d -> Svg msg
triangle2d attributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
    Svg.polygon (pointsAttribute [ p1, p2, p3 ] :: attributes) []


{-| Draw a `Polyline2d` as an SVG `<polyline>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#polyline" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#polyline`
</iframe>

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

-}
polyline2d : List (Attribute msg) -> Polyline2d -> Svg msg
polyline2d attributes polyline =
    let
        vertices =
            Polyline2d.vertices polyline
    in
    Svg.polyline (pointsAttribute vertices :: attributes) []


{-| Draw a `Polygon2d` as an SVG `<polygon>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#polygon" style="width: 120px; height: 70px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#polygon`
</iframe>

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

-}
polygon2d : List (Attribute msg) -> Polygon2d -> Svg msg
polygon2d attributes polygon =
    let
        loops =
            Polygon2d.outerLoop polygon :: Polygon2d.innerLoops polygon

        loopString loop =
            case loop of
                [] ->
                    ""

                _ ->
                    let
                        coordinateStrings =
                            loop
                                |> List.map
                                    (\point ->
                                        let
                                            ( x, y ) =
                                                Point2d.coordinates point

                                            xString =
                                                String.fromFloat x

                                            yString =
                                                String.fromFloat y
                                        in
                                        xString ++ " " ++ yString
                                    )
                    in
                    "M " ++ String.join " L " coordinateStrings ++ " Z"

        pathAttribute =
            Attributes.d (String.join " " (List.map loopString loops))
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw an `Arc2d` as an SVG `<path>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#arc" style="width: 100px; height: 110px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#arc`
</iframe>

    arc : Svg msg
    arc =
        Svg.arc2d
            [ Attributes.stroke "blue"
            , Attributes.strokeWidth "5"
            ]
            (Arc2d.with
                { centerPoint =
                    Point2d.fromCoordinates ( 100, 100 )
                , startPoint =
                    Point2d.fromCoordinates ( 150, 50 )
                , sweptAngle = degrees 90
                }
            )

-}
arc2d : List (Attribute msg) -> Arc2d -> Svg msg
arc2d attributes arc =
    let
        sweptAngle =
            Arc2d.sweptAngle arc

        maxSegmentAngle =
            2 * pi / 3

        numSegments =
            1 + floor (abs sweptAngle / maxSegmentAngle)

        sweepFlag =
            if sweptAngle >= 0 then
                "1"

            else
                "0"

        ( x0, y0 ) =
            Point2d.coordinates (Arc2d.startPoint arc)

        radius =
            Arc2d.radius arc

        radiusString =
            String.fromFloat radius

        moveCommand =
            [ "M"
            , String.fromFloat x0
            , String.fromFloat y0
            ]

        arcSegment parameterValue =
            let
                ( x, y ) =
                    Point2d.coordinates (Arc2d.pointOn arc parameterValue)
            in
            [ "A"
            , radiusString
            , radiusString
            , "0"
            , "0"
            , sweepFlag
            , String.fromFloat x
            , String.fromFloat y
            ]

        arcSegments =
            List.map arcSegment (ParameterValue.trailing numSegments)

        pathComponents =
            moveCommand ++ List.concat arcSegments

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw an `EllipticalArc2d` as an SVG `<path>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#ellipticalArc" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#ellipticalArc`
</iframe>

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

-}
ellipticalArc2d : List (Attribute msg) -> EllipticalArc2d -> Svg msg
ellipticalArc2d attributes arc =
    let
        sweptAngle =
            EllipticalArc2d.sweptAngle arc

        maxSegmentAngle =
            2 * pi / 3

        numSegments =
            1 + floor (abs sweptAngle / maxSegmentAngle)

        sweepFlag =
            if sweptAngle >= 0 then
                "1"

            else
                "0"

        ( x0, y0 ) =
            Point2d.coordinates (EllipticalArc2d.startPoint arc)

        xRadius =
            EllipticalArc2d.xRadius arc

        yRadius =
            EllipticalArc2d.yRadius arc

        xRadiusString =
            String.fromFloat xRadius

        yRadiusString =
            String.fromFloat yRadius

        xDirection =
            EllipticalArc2d.xDirection arc

        angleString =
            String.fromFloat (Direction2d.toAngle xDirection |> inDegrees)

        moveCommand =
            [ "M"
            , String.fromFloat x0
            , String.fromFloat y0
            ]

        arcSegment parameterValue =
            let
                ( x, y ) =
                    Point2d.coordinates
                        (EllipticalArc2d.pointOn arc parameterValue)
            in
            [ "A"
            , xRadiusString
            , yRadiusString
            , angleString
            , "0"
            , sweepFlag
            , String.fromFloat x
            , String.fromFloat y
            ]

        arcSegments =
            List.map arcSegment (ParameterValue.trailing numSegments)

        pathComponents =
            moveCommand ++ List.concat arcSegments

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw a `Circle2d` as an SVG `<circle>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#circle" style="width: 40px; height: 40px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#circle`
</iframe>

    circle : Svg msg
    circle =
        Svg.circle2d
            [ Attributes.fill "orange"
            , Attributes.stroke "blue"
            , Attributes.strokeWidth "2"
            ]
            (Circle2d.with
                { centerPoint =
                    Point2d.fromCoordinates ( 150, 150 )
                , radius = 10
                }
            )

-}
circle2d : List (Attribute msg) -> Circle2d -> Svg msg
circle2d attributes circle =
    let
        ( x, y ) =
            Point2d.coordinates (Circle2d.centerPoint circle)

        cx =
            Attributes.cx (String.fromFloat x)

        cy =
            Attributes.cy (String.fromFloat y)

        r =
            Attributes.r (String.fromFloat (Circle2d.radius circle))
    in
    Svg.circle (cx :: cy :: r :: attributes) []


{-| Draw an `Ellipse2d` as an SVG `<ellipse>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#ellipse" style="width: 140px; height: 100px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#ellipse`
</iframe>

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

-}
ellipse2d : List (Attribute msg) -> Ellipse2d -> Svg msg
ellipse2d attributes ellipse =
    let
        centerPoint =
            Ellipse2d.centerPoint ellipse

        ( x, y ) =
            Point2d.coordinates centerPoint

        cx =
            Attributes.cx (String.fromFloat x)

        cy =
            Attributes.cy (String.fromFloat y)

        rx =
            Attributes.rx (String.fromFloat (Ellipse2d.xRadius ellipse))

        ry =
            Attributes.ry (String.fromFloat (Ellipse2d.yRadius ellipse))

        angle =
            Direction2d.toAngle (Ellipse2d.xDirection ellipse)
    in
    Svg.ellipse (cx :: cy :: rx :: ry :: attributes) []
        |> rotateAround centerPoint angle


{-| Draw a quadratic spline as an SVG `<path>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#quadraticSpline" style="width: 130px; height: 130px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#quadraticSpline`
</iframe>

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

-}
quadraticSpline2d : List (Attribute msg) -> QuadraticSpline2d -> Svg msg
quadraticSpline2d attributes spline =
    let
        ( x1, y1 ) =
            Point2d.coordinates (QuadraticSpline2d.startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (QuadraticSpline2d.controlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (QuadraticSpline2d.endPoint spline)

        pathComponents =
            [ "M"
            , String.fromFloat x1
            , String.fromFloat y1
            , "Q"
            , String.fromFloat x2
            , String.fromFloat y2
            , String.fromFloat x3
            , String.fromFloat y3
            ]

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw a cubic spline as an SVG `<path>` with the given attributes.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#cubicSpline" style="width: 190px; height: 165px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#cubicSpline`
</iframe>

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

-}
cubicSpline2d : List (Attribute msg) -> CubicSpline2d -> Svg msg
cubicSpline2d attributes spline =
    let
        ( x1, y1 ) =
            Point2d.coordinates (CubicSpline2d.startPoint spline)

        ( x2, y2 ) =
            Point2d.coordinates (CubicSpline2d.startControlPoint spline)

        ( x3, y3 ) =
            Point2d.coordinates (CubicSpline2d.endControlPoint spline)

        ( x4, y4 ) =
            Point2d.coordinates (CubicSpline2d.endPoint spline)

        pathComponents =
            [ "M"
            , String.fromFloat x1
            , String.fromFloat y1
            , "C"
            , String.fromFloat x2
            , String.fromFloat y2
            , String.fromFloat x3
            , String.fromFloat y3
            , String.fromFloat x4
            , String.fromFloat y4
            ]

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw a bounding box as an SVG `<rect>` with the given attributes.
-}
boundingBox2d : List (Attribute msg) -> BoundingBox2d -> Svg msg
boundingBox2d attributes boundingBox =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema boundingBox

        x =
            Attributes.x (String.fromFloat minX)

        y =
            Attributes.y (String.fromFloat minY)

        width =
            Attributes.width (String.fromFloat (maxX - minX))

        height =
            Attributes.height (String.fromFloat (maxY - minY))
    in
    Svg.rect (x :: y :: width :: height :: attributes) []


{-| Scale arbitrary SVG around a given point by a given scale.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#scaled" style="width: 160px; height: 160px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#scaled`
</iframe>

    scaled : Svg msg
    scaled =
        let
            scales =
                [ 1.0, 1.5, 2.25 ]

            referencePoint =
                Point2d.fromCoordinates ( 100, 100 )

            referencePoint =
                Svg.circle2d [ Attributes.fill "black" ]
                    (Circle2d.with
                        { centerPoint = referencePoint
                        , radius = 3
                        }
                    )

            scaledCircle : Float -> Svg msg
            scaledCircle scale =
                Svg.scaleAbout referencePoint scale circle
        in
        Svg.g []
            (referencePoint
                :: List.map scaledCircle scales
            )

Note how _everything_ is scaled, including the stroke width of the circles. This
may or may not be what you want; if you wanted the same stroke width on all
circles, you could instead scale the `Circle2d` values themselves using
`Circle2d.scaleAbout` and then draw the scaled circles with a specific stroke
width using `Svg.circle2d`.

-}
scaleAbout : Point2d -> Float -> Svg msg -> Svg msg
scaleAbout point scale element =
    let
        ( px, py ) =
            Point2d.coordinates (Point2d.scaleAbout point scale Point2d.origin)

        components =
            List.map String.fromFloat [ scale, 0, 0, scale, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
    Svg.g [ Attributes.transform transform ] [ element ]


{-| Rotate arbitrary SVG around a given point by a given angle.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#rotated" style="width: 140px; height: 140px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#rotated`
</iframe>

    rotated : Svg msg
    rotated =
        let
            angles =
                List.range 0 9
                    |> List.map
                        (\n -> degrees 30 * toFloat n)

            referencePoint =
                Point2d.fromCoordinates ( 200, 150 )

            referencePoint =
                Svg.circle2d [ Attributes.fill "black" ]
                    (Circle2d.with
                        { centerPoint = referencePoint
                        , radius = 3
                        }
                    )

            rotatedCircle : Float -> Svg msg
            rotatedCircle angle =
                Svg.rotateAround referencePoint angle circle
        in
        Svg.g []
            (referencePoint
                :: List.map rotatedCircle angles
            )

-}
rotateAround : Point2d -> Float -> Svg msg -> Svg msg
rotateAround point angle element =
    let
        ( x, y ) =
            Point2d.coordinates point

        xString =
            String.fromFloat x

        yString =
            String.fromFloat y

        angleString =
            String.fromFloat (angle |> inDegrees)

        rotate =
            "rotate(" ++ angleString ++ " " ++ xString ++ " " ++ yString ++ ")"
    in
    Svg.g [ Attributes.transform rotate ] [ element ]


{-| Translate arbitrary SVG by a given displacement.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#translated" style="width: 128px; height: 230px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#translated`
</iframe>

    translated : Svg msg
    translated =
        Svg.g []
            [ polyline
            , polyline
                |> Svg.translateBy
                    (Vector2d.fromComponents ( 0, 40 ))
            , polyline
                |> Svg.translateBy
                    (Vector2d.fromComponents ( 5, -60 ))
            ]

-}
translateBy : Vector2d -> Svg msg -> Svg msg
translateBy vector element =
    let
        ( x, y ) =
            Vector2d.components vector

        xString =
            String.fromFloat x

        yString =
            String.fromFloat y

        translate =
            "translate(" ++ xString ++ " " ++ yString ++ ")"
    in
    Svg.g [ Attributes.transform translate ] [ element ]


{-| Mirror arbitrary SVG across a given axis.

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#mirrored" style="width: 230px; height: 280px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#mirrored`
</iframe>

    mirrored : Svg msg
    mirrored =
        let
            horizontalAxis =
                Axis2d.with
                    { originPoint =
                        Point2d.fromCoordinates ( 0, 220 )
                    , direction = Direction2d.x
                    }

            horizontalSegment =
                LineSegment2d.along horizontalAxis 50 250

            angledAxis =
                Axis2d.with
                    { originPoint =
                        Point2d.fromCoordinates ( 0, 150 )
                    , direction =
                        Direction2d.fromAngle (degrees -10)
                    }

            angledSegment =
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
                [ Svg.lineSegment2d [] horizontalSegment
                , Svg.lineSegment2d [] angledSegment
                ]
            ]

-}
mirrorAcross : Axis2d -> Svg msg -> Svg msg
mirrorAcross axis =
    placeIn (Frame2d.mirrorAcross axis Frame2d.xy)


{-| Convert SVG expressed in global coordinates to SVG expressed in coordinates
relative to a given reference frame. Using `relativeTo` can be useful for
transforming between model space and screen space - SVG coordinates start in the
top left, so positive Y is down, while in mathematical/geometric contexts
positive Y is usually up.

For example, you might develop an SVG scene in a coordinate system where X and Y
each range from 0 to 300 and positive Y is up. To turn this into a 300x300 SVG
drawing, first define the top-left SVG frame (coordinate system) in terms of
the model coordinate system:

    topLeftPoint =
        Point2d.fromCoordinates ( 0, 300 )

    topLeftFrame =
        Frame2d.atPoint topLeftPoint |> Frame2d.flipY

(As expressed in the model frame, the top-left SVG frame is at the point
(0, 300) and its Y direction is equal to the global negative Y direction.) If
`scene` is an SVG element representing your scene, you could then transform it
into top-left SVG window coordinates and render the result to HTML with

    Svg.svg
        [ Attributes.width "300"
        , Attributes.height "300"
        ]
        [ Svg.relativeTo topLeftFrame scene ]

-}
relativeTo : Frame2d -> Svg msg -> Svg msg
relativeTo frame =
    placeIn (Frame2d.relativeTo frame Frame2d.xy)


{-| Take SVG defined in local coordinates relative to a given reference frame,
and return that SVG expressed in global coordinates.

This can be useful for taking a chunk of SVG and 'stamping' it in different
positions with different orientations:

<iframe src="https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#placed" style="width: 225px; height: 180px" scrolling=no frameborder=0>
`https://ianmackenzie.github.io/elm-geometry-svg/1.0.0/DocumentationExamples.html#placed`
</iframe>

    placed : Svg msg
    placed =
        let
            vertices =
                [ Point2d.origin
                , Point2d.fromCoordinates ( 40, 0 )
                , Point2d.fromCoordinates ( 50, 25 )
                , Point2d.fromCoordinates ( 10, 25 )
                ]

            stamp =
                Svg.polygon2d
                    [ Attributes.fill "orange"
                    , Attributes.stroke "blue"
                    , Attributes.strokeWidth "2"
                    ]
                    (Polygon2d.singleLoop vertices)

            frames =
                [ Frame2d.atPoint
                    (Point2d.fromCoordinates ( 25, 25 ))
                , Frame2d.atPoint
                    (Point2d.fromCoordinates ( 100, 25 ))
                , Frame2d.atPoint
                    (Point2d.fromCoordinates ( 175, 25 ))
                    |> Frame2d.rotateBy (degrees 20)
                , Frame2d.atPoint
                    (Point2d.fromCoordinates ( 25, 150 ))
                , Frame2d.atPoint
                    (Point2d.fromCoordinates ( 100, 100 ))
                    |> Frame2d.rotateBy (degrees 20)
                , Frame2d.atPoint
                    (Point2d.fromCoordinates ( 150, 150 ))
                    |> Frame2d.rotateBy (degrees -30)
                ]
        in
        Svg.g []
            (frames
                |> List.map
                    (\frame -> Svg.placeIn frame stamp)
            )

-}
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
            List.map String.fromFloat [ x1, y1, x2, y2, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
    Svg.g [ Attributes.transform transform ] [ element ]
