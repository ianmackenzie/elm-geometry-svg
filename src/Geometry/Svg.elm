module Geometry.Svg exposing
    ( lineSegment2d, triangle2d, polyline2d, polygon2d, rectangle2d, arc2d, ellipticalArc2d, circle2d, ellipse2d, quadraticSpline2d, cubicSpline2d, boundingBox2d
    , scaleAbout, rotateAround, translateBy, mirrorAcross
    , at, at_
    , relativeTo, placeIn
    )

{-| Draw 2D `elm-geometry` values as SVG.


## A note on unit conversions

Since plain `Svg` values do not track what units or coordinate systems were used
to create them (unlike `elm-geometry` values such as `Point2d`s, `Rectangle2d`s
etc.), you lose some type safety when using the functions in this package to
convert from `elm-geometry` values to SVG. For example, the following doesn't
make sense (since the units don't match up) but will compile just fine since
the return value of the `Svg.lineSegment2d` function has no way of indicating
that it's in units of meters, not pixels:

    Svg.translateBy (Vector2d.pixels 300 400) <|
        Svg.lineSegment2d []
            (LineSegment.from
                (Point2d.meters 10 10)
                (Point2d.meters 20 20)
            )

Also note that because of how `elm-geometry` and `elm-units` works, something
like

    Svg.lineSegment2d [] <|
        LineSegment2d.from
            (Point2d.inches 1 2)
            (Point2d.inches 3 4)

will actually create a SVG element like

    <polyline points="0.0254,0.0508 0.0762,0.1016"/>

since all values get converted to and stored internally as base units (meters or
pixels). You can avoid this issue by using the [`at`](#at) or [`at_`](#at_)
functions to explicitly apply a conversion from (in this example) inches to
on-screen pixels:

    let
        lineSegment =
            LineSegment2d.from
                (Point2d.inches 1 2)
                (Point2d.inches 3 4)

        resolution =
            pixels 96 |> Quantity.per (Length.inches 1)
    in
    Svg.at resolution (Svg.lineSegment2d [] lineSegment)

which will result in something like

    <g transform="scale(3779.5276)">
        <polyline points="0.0254,0.0508 0.0762,0.1016"/>
    </g>

If you want to let the compiler check whether your units make sense, you could
change the above code to directly scale the line segment first and _then_
draw it:

    Svg.lineSegment2d [] <|
        LineSegment2d.at resolution lineSegment

This will result in simply

    <polyline points="96,192 288,384"/>

which should be visually identical to the `<g>` version but will let the Elm
compiler check whether the given conversion factor is valid to apply to the
given line segment. The downside to this approach is that _changing_ the
resolution/scale factor, for example when zooming, then transforms the actual
geometry before rendering it to SVG instead of rendering the same geometry to
SVG and just changing what transformation is applied to it. For large, complex
geometry, transforming the geometry itself is likely to be expensive, while just
changing a transformation should be cheap.


## Reading this documentation

For the examples, assume that the following imports are present:

    import Svg exposing (Svg)
    import Svg.Attributes as Attributes
    import Geometry.Svg as Svg
    import Angle
    import Pixels exposing (pixels)

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

@docs lineSegment2d, triangle2d, polyline2d, polygon2d, rectangle2d, arc2d, ellipticalArc2d, circle2d, ellipse2d, quadraticSpline2d, cubicSpline2d, boundingBox2d


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
        (Angle.degrees 30)
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


# Unit conversions

@docs at, at_


# Coordinate conversions

These functions allow `elm-geometry` coordinate conversion transformations to be
applied to arbitrary SVG elements. Note that the same caveats as unit
conversions apply: you'll have to be careful to verify yourself that the
coordinate conversions actually make sense, since the compiler will be unable to
check for you.

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import LineSegment2d exposing (LineSegment2d)
import Parameter1d
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity(..), Rate)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attributes
import Triangle2d exposing (Triangle2d)
import Vector2d exposing (Vector2d)


toString : Quantity Float units -> String
toString (Quantity quantity) =
    String.fromFloat quantity


coordinatesString : Point2d units coordinates -> String
coordinatesString point =
    let
        { x, y } =
            Point2d.unwrap point
    in
    String.fromFloat x ++ "," ++ String.fromFloat y


pointsAttribute : List (Point2d units coordinates) -> Attribute msg
pointsAttribute points =
    Attributes.points (String.join " " (List.map coordinatesString points))


{-| Draw a `LineSegment2d` as an SVG `<polyline>` with the given attributes.

![Line segment](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/lineSegment2d.svg)

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

-}
lineSegment2d : List (Attribute msg) -> LineSegment2d units coordinates -> Svg msg
lineSegment2d attributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment
    in
    Svg.polyline (pointsAttribute [ p1, p2 ] :: attributes) []


{-| Draw a `Triangle2d` as an SVG `<polygon>` with the given attributes.

![Triangle](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/triangle2d.svg)

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

-}
triangle2d : List (Attribute msg) -> Triangle2d units coordinates -> Svg msg
triangle2d attributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
    Svg.polygon (pointsAttribute [ p1, p2, p3 ] :: attributes) []


{-| Draw a `Polyline2d` as an SVG `<polyline>` with the given attributes.

![Polyline](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/polyline2d.svg)

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

-}
polyline2d : List (Attribute msg) -> Polyline2d units coordinates -> Svg msg
polyline2d attributes polyline =
    let
        vertices =
            Polyline2d.vertices polyline
    in
    Svg.polyline (pointsAttribute vertices :: attributes) []


{-| Draw a `Polygon2d` as an SVG `<polygon>` with the given attributes.

![Polygon](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/polygon2d.svg)

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

-}
polygon2d : List (Attribute msg) -> Polygon2d units coordinates -> Svg msg
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
                                            { x, y } =
                                                Point2d.unwrap point
                                        in
                                        String.fromFloat x ++ " " ++ String.fromFloat y
                                    )
                    in
                    "M " ++ String.join " L " coordinateStrings ++ " Z"

        pathAttribute =
            Attributes.d (String.join " " (List.map loopString loops))
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw a `Rectangle2d` as an SVG `<rectangle>` with the given attributes.

![Rectangle](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/rectangle2d.svg)

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
            (Rectangle2d.centeredOn axes
                ( pixels 120, pixels 80 )
            )

-}
rectangle2d : List (Attribute msg) -> Rectangle2d units coordinates -> Svg msg
rectangle2d attributes rectangle =
    let
        ( width, height ) =
            Rectangle2d.dimensions rectangle
    in
    Svg.rect
        (Attributes.width (toString width)
            :: Attributes.height (toString height)
            :: Attributes.x (toString (Quantity.multiplyBy -0.5 width))
            :: Attributes.y (toString (Quantity.multiplyBy -0.5 height))
            :: attributes
        )
        []
        |> placeIn (Rectangle2d.axes rectangle)


{-| Draw an `Arc2d` as an SVG `<path>` with the given attributes.

![Arc](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/arc2d.svg)

    arc : Svg msg
    arc =
        Svg.arc2d
            [ Attributes.stroke "blue"
            , Attributes.strokeWidth "5"
            ]
            (Point2d.pixels 150 50
                |> Arc2d.sweptAround
                    (Point2d.pixels 100 100)
                    (Angle.degrees 90)
            )

-}
arc2d : List (Attribute msg) -> Arc2d units coordinates -> Svg msg
arc2d attributes arc =
    let
        sweptAngle =
            Arc2d.sweptAngle arc

        maxSegmentAngle =
            Angle.turns (1 / 3)

        numSegments =
            1 + floor (abs (Quantity.ratio sweptAngle maxSegmentAngle))

        sweepFlag =
            if sweptAngle |> Quantity.greaterThanOrEqualTo Quantity.zero then
                "1"

            else
                "0"

        p0 =
            Point2d.unwrap (Arc2d.startPoint arc)

        radius =
            Arc2d.radius arc

        radiusString =
            toString radius

        moveCommand =
            [ "M"
            , String.fromFloat p0.x
            , String.fromFloat p0.y
            ]

        arcSegment parameterValue =
            let
                { x, y } =
                    Point2d.unwrap (Arc2d.pointOn arc parameterValue)
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
            Parameter1d.trailing numSegments arcSegment

        pathComponents =
            moveCommand ++ List.concat arcSegments

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw an `EllipticalArc2d` as an SVG `<path>` with the given attributes.

![Elliptical arc](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/ellipticalArc2d.svg)

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
                , xRadius = pixels 50
                , yRadius = pixels 100
                , startAngle = Angle.degrees 0
                , sweptAngle = Angle.degrees 180
                }
            )

-}
ellipticalArc2d : List (Attribute msg) -> EllipticalArc2d units coordinates -> Svg msg
ellipticalArc2d attributes arc =
    let
        sweptAngle =
            EllipticalArc2d.sweptAngle arc

        maxSegmentAngle =
            Angle.turns (1 / 3)

        numSegments =
            1 + floor (abs (Quantity.ratio sweptAngle maxSegmentAngle))

        sweepFlag =
            if sweptAngle |> Quantity.greaterThanOrEqualTo Quantity.zero then
                "1"

            else
                "0"

        p0 =
            Point2d.unwrap (EllipticalArc2d.startPoint arc)

        xRadius =
            EllipticalArc2d.xRadius arc

        yRadius =
            EllipticalArc2d.yRadius arc

        xDirection =
            EllipticalArc2d.xDirection arc

        angleString =
            String.fromFloat (Angle.inDegrees (Direction2d.toAngle xDirection))

        moveCommand =
            [ "M"
            , String.fromFloat p0.x
            , String.fromFloat p0.y
            ]

        arcSegment parameterValue =
            let
                { x, y } =
                    Point2d.unwrap
                        (EllipticalArc2d.pointOn arc parameterValue)
            in
            [ "A"
            , toString xRadius
            , toString yRadius
            , angleString
            , "0"
            , sweepFlag
            , String.fromFloat x
            , String.fromFloat y
            ]

        arcSegments =
            Parameter1d.trailing numSegments arcSegment

        pathComponents =
            moveCommand ++ List.concat arcSegments

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw a `Circle2d` as an SVG `<circle>` with the given attributes.

![Circle](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/circle2d.svg)

    circle : Svg msg
    circle =
        Svg.circle2d
            [ Attributes.fill "orange"
            , Attributes.stroke "blue"
            , Attributes.strokeWidth "2"
            ]
            (Circle2d.withRadius (pixels 10)
                (Point2d.pixels 150 150)
            )

-}
circle2d : List (Attribute msg) -> Circle2d units coordinates -> Svg msg
circle2d attributes circle =
    let
        { x, y } =
            Point2d.unwrap (Circle2d.centerPoint circle)

        cx =
            Attributes.cx (String.fromFloat x)

        cy =
            Attributes.cy (String.fromFloat y)

        r =
            Attributes.r (toString (Circle2d.radius circle))
    in
    Svg.circle (cx :: cy :: r :: attributes) []


{-| Draw an `Ellipse2d` as an SVG `<ellipse>` with the given attributes.

![Ellipse](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/ellipse2d.svg)

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
                , xRadius = pixels 60
                , yRadius = pixels 30
                }
            )

-}
ellipse2d : List (Attribute msg) -> Ellipse2d units coordinates -> Svg msg
ellipse2d attributes ellipse =
    let
        centerPoint =
            Ellipse2d.centerPoint ellipse

        { x, y } =
            Point2d.unwrap centerPoint

        cx =
            Attributes.cx (String.fromFloat x)

        cy =
            Attributes.cy (String.fromFloat y)

        rx =
            Attributes.rx (toString (Ellipse2d.xRadius ellipse))

        ry =
            Attributes.ry (toString (Ellipse2d.yRadius ellipse))

        angle =
            Direction2d.toAngle (Ellipse2d.xDirection ellipse)
    in
    Svg.ellipse (cx :: cy :: rx :: ry :: attributes) []
        |> rotateAround centerPoint angle


{-| Draw a quadratic spline as an SVG `<path>` with the given attributes.

![Quadratic spline](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/quadraticSpline2d.svg)

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
                [ firstControlPoint
                , secondControlPoint
                , thirdControlPoint
                ]

            drawPoint point =
                Svg.circle2d [] <|
                    Circle2d.withRadius (pixels 3) point

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

-}
quadraticSpline2d : List (Attribute msg) -> QuadraticSpline2d units coordinates -> Svg msg
quadraticSpline2d attributes spline =
    let
        p1 =
            Point2d.unwrap (QuadraticSpline2d.firstControlPoint spline)

        p2 =
            Point2d.unwrap (QuadraticSpline2d.secondControlPoint spline)

        p3 =
            Point2d.unwrap (QuadraticSpline2d.thirdControlPoint spline)

        pathComponents =
            [ "M"
            , String.fromFloat p1.x
            , String.fromFloat p1.y
            , "Q"
            , String.fromFloat p2.x
            , String.fromFloat p2.y
            , String.fromFloat p3.x
            , String.fromFloat p3.y
            ]

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw a cubic spline as an SVG `<path>` with the given attributes.

![Cubic spline](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/cubicSpline2d.svg)

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
                    Circle2d.withRadius (pixels 3) point
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

-}
cubicSpline2d : List (Attribute msg) -> CubicSpline2d units coordinates -> Svg msg
cubicSpline2d attributes spline =
    let
        p1 =
            Point2d.unwrap (CubicSpline2d.firstControlPoint spline)

        p2 =
            Point2d.unwrap (CubicSpline2d.secondControlPoint spline)

        p3 =
            Point2d.unwrap (CubicSpline2d.thirdControlPoint spline)

        p4 =
            Point2d.unwrap (CubicSpline2d.fourthControlPoint spline)

        pathComponents =
            [ "M"
            , String.fromFloat p1.x
            , String.fromFloat p1.y
            , "C"
            , String.fromFloat p2.x
            , String.fromFloat p2.y
            , String.fromFloat p3.x
            , String.fromFloat p3.y
            , String.fromFloat p4.x
            , String.fromFloat p4.y
            ]

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
    Svg.path (pathAttribute :: attributes) []


{-| Draw a bounding box as an SVG `<rect>` with the given attributes.
-}
boundingBox2d : List (Attribute msg) -> BoundingBox2d units coordinates -> Svg msg
boundingBox2d attributes boundingBox =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema boundingBox

        x =
            Attributes.x (toString minX)

        y =
            Attributes.y (toString minY)

        width =
            Attributes.width (toString (maxX |> Quantity.minus minX))

        height =
            Attributes.height (toString (maxY |> Quantity.minus minY))
    in
    Svg.rect (x :: y :: width :: height :: attributes) []


{-| Scale arbitrary SVG around a given point by a given scale.

![Scaled circles](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/scaleAbout.svg)

    scaled : Svg msg
    scaled =
        let
            scales =
                [ 1.0, 1.5, 2.25 ]

            referencePoint =
                Point2d.pixels 100 100

            referencePoint =
                Svg.circle2d [ Attributes.fill "black" ] <|
                    Circle2d.withRadius (pixels 3)
                        referencePoint

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
scaleAbout : Point2d units coordinates -> Float -> Svg msg -> Svg msg
scaleAbout point scale element =
    let
        { x, y } =
            Point2d.unwrap (Point2d.scaleAbout point scale Point2d.origin)

        scaleString =
            String.fromFloat scale

        components =
            [ scaleString
            , "0"
            , "0"
            , scaleString
            , String.fromFloat x
            , String.fromFloat y
            ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
    Svg.g [ Attributes.transform transform ] [ element ]


{-| Rotate arbitrary SVG around a given point by a given angle.

![Rotated circles](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/rotateAround.svg)

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

            referenceCircle =
                Svg.circle2d [ Attributes.fill "black" ] <|
                    (Circle2d.withRadius (pixels 3)
                        referencePoint
                    )

            rotatedCircle : Float -> Svg msg
            rotatedCircle angle =
                Svg.rotateAround referencePoint angle circle
        in
        Svg.g [] <|
            referenceCircle
                :: List.map rotatedCircle angles

-}
rotateAround : Point2d units coordinates -> Angle -> Svg msg -> Svg msg
rotateAround point angle element =
    let
        { x, y } =
            Point2d.unwrap point

        angleString =
            String.fromFloat (Angle.inDegrees angle)

        rotate =
            "rotate(" ++ angleString ++ " " ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")"
    in
    Svg.g [ Attributes.transform rotate ] [ element ]


{-| Translate arbitrary SVG by a given displacement.

![Translated polylines](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/translateBy.svg)

    translated : Svg msg
    translated =
        Svg.g []
            [ polyline
            , polyline
                |> Svg.translateBy (Vector2d.pixels 0 40)
            , polyline
                |> Svg.translateBy (Vector2d.pixels 5 -60)
            ]

-}
translateBy : Vector2d units coordinates -> Svg msg -> Svg msg
translateBy vector element =
    let
        { x, y } =
            Vector2d.unwrap vector

        translate =
            "translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")"
    in
    Svg.g [ Attributes.transform translate ] [ element ]


{-| Mirror arbitrary SVG across a given axis.

![Mirrored polygons](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/mirrorAcross.svg)

    mirrored : Svg msg
    mirrored =
        let
            horizontalAxis =
                Axis2d.through (Point2d.pixels 0 220)
                    Direction2d.x

            horizontalSegment =
                LineSegment2d.along horizontalAxis
                    (pixels 50)
                    (pixels 250)

            angledAxis =
                Axis2d.through (Point2d.pixels 0 150)
                    (Direction2d.degrees -10)

            angledSegment =
                LineSegment2d.along angledAxis
                    (pixels 50)
                    (pixels 250)
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
mirrorAcross : Axis2d units coordinates -> Svg msg -> Svg msg
mirrorAcross axis =
    placeIn (Frame2d.mirrorAcross axis Frame2d.atOrigin)


{-| Take some SVG that is assumed to be defined in `units1` and convert it to
one defined in `units2`, given a conversion factor defined as `units2` per
`units1`. This is equivalent to a multiplication by that conversion factor. For
example, you could create an SVG element in units of meters and then scale it to
a certain on-screen size:

    let
        pixelsPerMeter =
            pixels 10 |> Quantity.per (Length.meters 1)
    in
    Svg.at pixelsPerMeter <|
        Svg.lineSegment2d [] <|
            LineSegment2d.from
                (Point2d.meters 1 2)
                (Point2d.meters 3 4)

Note that you can mix and match what specific units you use as long as the
underlying units match up. For example, something like this would work out since
both `centimeters` and `feet` both have the same underlying units (meters):

    let
        onePixelPerCentimeter =
            pixels 1 |> Quantity.per (Length.centimeters 1)
    in
    Svg.at onePixelPerCentimeter <|
        Svg.lineSegment2d [] <|
            LineSegment2d.from
                (Point2d.feet 1 2)
                (Point2d.feet 3 4)

This will result in something like:

    <g transform="scale(100)">
        <polyline points="0.3048,0.6096 0.9144,1.2192"/>
    </g>

(One foot is 30.48 centimeters or 0.3048 meters.)

-}
at : Quantity Float (Rate units2 units1) -> Svg msg -> Svg msg
at (Quantity rate) svg =
    scaleAbout Point2d.origin rate svg


{-| Similar to `at`, but if you have an 'inverse' rate instead. For example:

    let
        metersPerPixels =
            Length.millimeters 1 |> Quantity.per (pixels 1)
    in
    Svg.at_ metersPerPixel <|
        Svg.lineSegment2d [] <|
            LineSegment2d.from
                (Point2d.centimeters 1 2)
                (Point2d.centimeters 3 4)

This would be equivalent to

    let
        pixelsPerMeter =
            pixels 1 |> Quantity.per (Length.millimeters 1)
    in
    Svg.at pixelsPerMeter <|
        Svg.lineSegment2d [] <|
            LineSegment2d.from
                (Point2d.centimeters 1 2)
                (Point2d.centimeters 3 4)

-}
at_ : Quantity Float (Rate units1 units2) -> Svg msg -> Svg msg
at_ (Quantity rate) svg =
    scaleAbout Point2d.origin (1 / rate) svg


{-| Convert SVG expressed in global coordinates to SVG expressed in coordinates
relative to a given reference frame. Using `relativeTo` can be useful for
transforming between model space and screen space - SVG coordinates start in the
top left, so positive Y is down, while in mathematical/geometric contexts
positive Y is usually up.

For example, you might develop an SVG scene in a coordinate system where X and Y
each range from 0 to 300 and positive Y is up. To turn this into a 300x300 SVG
drawing, first define the top-left SVG frame (coordinate system) in terms of
the model coordinate system:

    topLeftFrame =
        Frame2d.atPoint (Point2d.pixels 0 300)
            |> Frame2d.reverseY

(As expressed in the model frame, the top-left SVG frame is at the point
(0, 300) and its Y direction is equal to the global negative Y direction.) If
`scene` is an SVG element representing your scene, you could then transform it
into top-left SVG window coordinates and render the result to HTML with

    Svg.svg
        [ Attributes.width "300"
        , Attributes.height "300"
        ]
        [ Svg.relativeTo topLeftFrame scene ]

Note however that if you do this, any text you added will come out upside down!
If, like me, you really prefer to use a Y-up coordinate system when drawing,
you could write a little helper function that rendered text at a particular
point and then flipped it upside down (mirrored it across a horizontal axis) so
that your final `relativeTo` would flip it back to right side up. Something
like:

    drawText :
        List (Svg.Attribute msg)
        -> Point2d units coordinates
        -> String
        -> Svg msg
    drawText givenAttributes position content =
        let
            { x, y } =
                Point2d.unwrap position

            positionAttributes =
                [ Svg.Attributes.x (String.fromFloat x)
                , Svg.Attributes.y (String.fromFloat y)
                ]
        in
        Svg.text_ (positionAttributes ++ givenAttributes)
            [ Svg.text content ]
            |> Svg.mirrorAcross
                (Axis2d.through position Direction2d.x)

-}
relativeTo : Frame2d units coordinates defines -> Svg msg -> Svg msg
relativeTo frame =
    placeIn (Frame2d.relativeTo (Frame2d.copy frame) Frame2d.atOrigin)


{-| Take SVG defined in local coordinates relative to a given reference frame,
and return that SVG expressed in global coordinates.

This can be useful for taking a chunk of SVG and 'stamping' it in different
positions with different orientations:

![Placed polygons](https://ianmackenzie.github.io/elm-geometry-svg/2.0.0/images/placeIn.svg)

    placed : Svg msg
    placed =
        let
            vertices =
                [ Point2d.origin
                , Point2d.pixels 40 0
                , Point2d.pixels 50 25
                , Point2d.pixels 10 25
                ]

            stamp =
                Svg.polygon2d
                    [ Attributes.fill "orange"
                    , Attributes.stroke "blue"
                    , Attributes.strokeWidth "2"
                    ]
                    (Polygon2d.singleLoop vertices)

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
        Svg.g []
            (frames
                |> List.map
                    (\frame -> Svg.placeIn frame stamp)
            )

-}
placeIn : Frame2d units coordinates defines -> Svg msg -> Svg msg
placeIn frame element =
    let
        p =
            Point2d.unwrap (Frame2d.originPoint frame)

        d1 =
            Direction2d.unwrap (Frame2d.xDirection frame)

        d2 =
            Direction2d.unwrap (Frame2d.yDirection frame)

        components =
            [ String.fromFloat d1.x
            , String.fromFloat d1.y
            , String.fromFloat d2.x
            , String.fromFloat d2.y
            , String.fromFloat p.x
            , String.fromFloat p.y
            ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
    Svg.g [ Attributes.transform transform ] [ element ]
