module OpenSolid.Svg
    exposing
        ( render2d
        , VectorOptions
        , vector2d
        , DirectionOptions
        , direction2d
        , PointOptions
        , point2d
        , lineSegment2d
        , triangle2d
        , polyline2d
        , polygon2d
        , arc2d
        , circle2d
        , quadraticSpline2d
        , cubicSpline2d
        , text2d
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| Various OpenSolid-related SVG functionality:

  - [Draw](#geometry) OpenSolid 2D geometry as SVG elements
  - [Transform](#transformations) arbitrary SVG elements using standard
    OpenSolid transformation functions
  - [Convert](#coordinate-transformations) SVG between different coordinate
    systems


## Reading this documentation

For the examples, assume that the following imports are present:

    import Svg exposing (Svg)
    import Svg.Attributes as Attributes
    import OpenSolid.Svg as Svg
    import OpenSolid.Geometry.Types exposing (..)

Also assume that any necessary OpenSolid modules have been imported using the
following format:

    import OpenSolid.Point2d as Point2d

All examples use a Y-up coordinate system instead of SVG's Y-down (window)
coordinate system; they were all rendered with a final [`render2d`](#render2d)
call to flip the example 'upside down' for display.


# Rendering

@docs render2d


# Primitives

The basic primitive types (points, directions, vectors) don't have an obvious
mapping to SVG, but the functions in this section attempt to provide a simple
default way to draw each of them. If you want more customization, you will
likely need to write your own custom helper functions (perhaps using other
functions from this package); for example, a 'crosshair' style point might be
formed from a combination of `circle2d` and `lineSegment2d` calls.

@docs VectorOptions, vector2d, DirectionOptions, direction2d, PointOptions, point2d


# Geometry

These functions turn OpenSolid values into SVG elements with geometric
attributes such as `points` and `transform` set appropriately. Each function
also accepts a list of additional SVG attributes such as `fill` or `stroke` that
should be added to the resulting element.

@docs lineSegment2d, triangle2d, polyline2d, polygon2d, arc2d, circle2d, quadraticSpline2d, cubicSpline2d


# Text

@docs text2d


# Transformations

These functions allow you to use all the normal OpenSolid 2D transformations on
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

    Svg.rotateAround Point2d.origin (degrees 30)
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

Similar to the above transformations, these functions allow OpenSolid coordinate
conversion transformations to be applied to arbitrary SVG elements.

@docs relativeTo, placeIn

-}

import Svg exposing (Svg, Attribute)
import Html exposing (Html)
import Svg.Attributes as Attributes
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Polyline2d as Polyline2d
import OpenSolid.Polygon2d as Polygon2d
import OpenSolid.Arc2d as Arc2d
import OpenSolid.Circle2d as Circle2d
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.BoundingBox2d as BoundingBox2d


{-| Render some SVG to an HTML `<svg>` element, clipping to the given bounding
box. It is assumed that the SVG is already in pixel units, so the bounding box
is also used to set the width and height of the resulting `<svg>` element.

In addition, it is assumed that the SVG has been drawn in a coordinate system
with positive X to the right and positive Y up, so this function will flip the
coordinate system so that coordinates start at the top-left corner as required.

-}
render2d : BoundingBox2d -> Svg msg -> Html msg
render2d boundingBox svg =
    let
        { minX, maxY } =
            BoundingBox2d.extrema boundingBox

        topLeftFrame =
            Frame2d
                { originPoint = Point2d ( minX, maxY )
                , xDirection = Direction2d.positiveX
                , yDirection = Direction2d.negativeY
                }

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox
    in
        Svg.svg
            [ Attributes.width (toString width)
            , Attributes.height (toString height)
            ]
            [ relativeTo topLeftFrame svg ]


coordinatesString : Point2d -> String
coordinatesString point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        toString x ++ "," ++ toString y


pointsAttribute : List Point2d -> Attribute msg
pointsAttribute points =
    Attributes.points (String.join " " (List.map coordinatesString points))


{-| Options type used by the `vector2d` function.
-}
type alias VectorOptions msg =
    { tipWidth : Float
    , tipLength : Float
    , stemAttributes : List (Attribute msg)
    , tipAttributes : List (Attribute msg)
    , groupAttributes : List (Attribute msg)
    }


{-| Draw a vector with the given options, starting from the given base point.
Nothing will be drawn if the vector length is zero. Vectors are drawn as an
arrow with a stem line and a tip triangle and have the following options:

  - `tipWidth` is the width of the tip triangle
  - `tipLength` is the length of the tip triangle
  - `stemAttributes` are applied to the stem line
  - `tipAttributes` are applied to the tip triangle
  - `groupAttributes` are applied to the group formed by the stem line and
    tip triangle (the entire arrow)

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#vector" style="width: 120px; height: 120px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#vector>

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

-}
vector2d : VectorOptions msg -> Point2d -> Vector2d -> Svg msg
vector2d options basePoint vector =
    case Vector2d.lengthAndDirection vector of
        Just ( length, direction ) ->
            let
                frame =
                    Frame2d
                        { originPoint = basePoint
                        , xDirection = direction
                        , yDirection = Direction2d.perpendicularTo direction
                        }

                tipLength =
                    options.tipLength

                tipWidth =
                    options.tipWidth

                tipPoint =
                    Point2d.in_ frame ( length, 0 )

                tipBasePoint =
                    Point2d.in_ frame ( length - tipLength, 0 )

                leftPoint =
                    Point2d.in_ frame ( length - tipLength, tipWidth / 2 )

                rightPoint =
                    Point2d.in_ frame ( length - tipLength, -tipWidth / 2 )

                stem =
                    LineSegment2d ( basePoint, tipBasePoint )

                tip =
                    Triangle2d ( rightPoint, tipPoint, leftPoint )
            in
                Svg.g options.groupAttributes
                    [ lineSegment2d options.stemAttributes stem
                    , triangle2d options.tipAttributes tip
                    ]

        Nothing ->
            Svg.text ""


{-| Options type used by the `direction2d` function.
-}
type alias DirectionOptions msg =
    { length : Float
    , tipWidth : Float
    , tipLength : Float
    , stemAttributes : List (Attribute msg)
    , tipAttributes : List (Attribute msg)
    , groupAttributes : List (Attribute msg)
    }


{-| Draw a direction with the given options, starting from the given base point.
Options are the same as for `vector2d`, with the addition of `length` which
controls the length of the displayed arrow (since directions have no length of
their own).

The intent is that for simple cases, you can partially apply this function to
create a helper function with your preferred display options 'baked in':

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#direction" style="width: 120px; height: 120px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#direction>

    drawDirection : Point2d -> Direction2d -> Svg Never
    drawDirection =
        Svg.direction2d
            { length = 50
            , tipLength = 7
            , tipWidth = 7
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

-}
direction2d : DirectionOptions msg -> Point2d -> Direction2d -> Svg msg
direction2d options basePoint direction =
    vector2d
        { tipLength = options.tipLength
        , tipWidth = options.tipWidth
        , stemAttributes = options.stemAttributes
        , tipAttributes = options.tipAttributes
        , groupAttributes = options.groupAttributes
        }
        basePoint
        (Vector2d.in_ direction options.length)


{-| Options type used in the `point2d` function.
-}
type alias PointOptions msg =
    { radius : Float
    , attributes : List (Attribute msg)
    }


{-| Draw a point as a circle with the given options.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#point" style="width: 120px; height: 120px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#point>

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

-}
point2d : PointOptions msg -> Point2d -> Svg msg
point2d options point =
    circle2d options.attributes
        (Circle2d { centerPoint = point, radius = options.radius })


{-| Draw a `LineSegment2d` as an SVG `<polyline>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#lineSegment" style="width: 120px; height: 120px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#lineSegment>

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

-}
lineSegment2d : List (Attribute msg) -> LineSegment2d -> Svg msg
lineSegment2d attributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment
    in
        Svg.polyline (pointsAttribute [ p1, p2 ] :: attributes) []


{-| Draw a `Triangle2d` as an SVG `<polygon>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#triangle" style="width: 120px; height: 120px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#triangle>

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

-}
triangle2d : List (Attribute msg) -> Triangle2d -> Svg msg
triangle2d attributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
        Svg.polygon (pointsAttribute [ p1, p2, p3 ] :: attributes) []


{-| Draw a `Polyline2d` as an SVG `<polyline>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#polyline" style="width: 120px; height: 120px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#polyline>

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

-}
polyline2d : List (Attribute msg) -> Polyline2d -> Svg msg
polyline2d attributes polyline =
    let
        vertices =
            Polyline2d.vertices polyline
    in
        Svg.polyline (pointsAttribute vertices :: attributes) []


{-| Draw a `Polygon2d` as an SVG `<polygon>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#polygon" style="width: 120px; height: 70px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#polygon>

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

-}
polygon2d : List (Attribute msg) -> Polygon2d -> Svg msg
polygon2d attributes polygon =
    let
        vertices =
            Polygon2d.vertices polygon
    in
        Svg.polygon (pointsAttribute vertices :: attributes) []


{-| Draw an `Arc2d` as an SVG `<path>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#arc" style="width: 100px; height: 110px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#arc>

    arcSvg : Svg Never
    arcSvg =
        Svg.arc2d
            [ Attributes.stroke "blue"
            , Attributes.strokeWidth "5"
            ]
            (Arc2d
                { centerPoint = Point2d ( 100, 100 )
                , startPoint = Point2d ( 150, 50 )
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
            toString radius

        moveCommand =
            [ "M"
            , toString x0
            , toString y0
            ]

        arcSegment index =
            let
                t =
                    toFloat index / toFloat numSegments

                ( x, y ) =
                    Point2d.coordinates (Arc2d.point arc t)
            in
                [ "A"
                , radiusString
                , radiusString
                , "0"
                , "0"
                , sweepFlag
                , toString x
                , toString y
                ]

        arcSegments =
            List.map arcSegment (List.range 1 numSegments)

        pathComponents =
            moveCommand ++ List.concat arcSegments

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
        Svg.path (pathAttribute :: attributes) []


{-| Draw a `Circle2d` as an SVG `<circle>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#circle" style="width: 40px; height: 40px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#circle>

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

-}
circle2d : List (Attribute msg) -> Circle2d -> Svg msg
circle2d attributes circle =
    let
        ( x, y ) =
            Point2d.coordinates (Circle2d.centerPoint circle)

        cx =
            Attributes.cx (toString x)

        cy =
            Attributes.cy (toString y)

        r =
            Attributes.r (toString (Circle2d.radius circle))
    in
        Svg.circle (cx :: cy :: r :: attributes) []


{-| Draw a quadratic spline as an SVG `<path>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#quadraticSpline" style="width: 130px; height: 130px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#quadraticSpline>

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

-}
quadraticSpline2d : List (Attribute msg) -> QuadraticSpline2d -> Svg msg
quadraticSpline2d attributes spline =
    let
        ( p1, p2, p3 ) =
            QuadraticSpline2d.controlPoints spline

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

        pathComponents =
            [ "M"
            , toString x1
            , toString y1
            , "Q"
            , toString x2
            , toString y2
            , toString x3
            , toString y3
            ]

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
        Svg.path (pathAttribute :: attributes) []


{-| Draw a cubic spline as an SVG `<path>` with the given attributes.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#cubicSpline" style="width: 190px; height: 165px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#cubicSpline>

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

-}
cubicSpline2d : List (Attribute msg) -> CubicSpline2d -> Svg msg
cubicSpline2d attributes spline =
    let
        ( p1, p2, p3, p4 ) =
            CubicSpline2d.controlPoints spline

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3

        ( x4, y4 ) =
            Point2d.coordinates p4

        pathComponents =
            [ "M"
            , toString x1
            , toString y1
            , "C"
            , toString x2
            , toString y2
            , toString x3
            , toString y3
            , toString x4
            , toString y4
            ]

        pathAttribute =
            Attributes.d (String.join " " pathComponents)
    in
        Svg.path (pathAttribute :: attributes) []


{-| Draw a string of text with the given attributes at the given point. You can
use the SVG [`textAnchor`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-anchor)
and [`alignmentBaseline`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignment-baseline)
attributes to align the text relative to the given point as desired.

The wrinkle is that this function assumes that you are constructing your diagram
in a coordinate system where positive X is to the right and positive Y is up. As
a result, it will actually draw the text *upside down*, assuming that you will
eventually flip your entire diagram upside down again to convert it to the
Y-down coordinate system used by SVG (perhaps by using `render2d`).

Note that you can apply all the usual transformations to SVG text just like any
other SVG element!

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#text" style="width: 220px; height: 170px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#text>

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

-}
text2d : List (Attribute msg) -> Point2d -> String -> Svg msg
text2d attributes basePoint text =
    let
        ( x, y ) =
            Point2d.coordinates basePoint

        mirrorAxis =
            Axis2d
                { originPoint = basePoint
                , direction = Direction2d.x
                }

        xAttribute =
            Attributes.x (toString x)

        yAttribute =
            Attributes.y (toString y)
    in
        Svg.text_ (xAttribute :: yAttribute :: attributes) [ Svg.text text ]
            |> mirrorAcross mirrorAxis


{-| Scale arbitrary SVG around a given point by a given scale.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#scaled" style="width: 160px; height: 160px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#scaled>

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

Note how *everything* is scaled, including the stroke width of the circles. This
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
            List.map toString [ scale, 0, 0, scale, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
        Svg.g [ Attributes.transform transform ] [ element ]


{-| Rotate arbitrary SVG around a given point by a given angle.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#rotated" style="width: 140px; height: 140px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#rotated>

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

-}
rotateAround : Point2d -> Float -> Svg msg -> Svg msg
rotateAround point angle =
    placeIn (Frame2d.rotateAround point angle Frame2d.xy)


{-| Translate arbitrary SVG by a given displacement.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#translated" style="width: 128px; height: 230px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#translated>

    translatedSvg : Svg Never
    translatedSvg =
        Svg.g []
            [ polylineSvg
            , Svg.translateBy (Vector2d ( 0, 40 )) polylineSvg
            , Svg.translateBy (Vector2d ( 5, -60 )) polylineSvg
            ]

-}
translateBy : Vector2d -> Svg msg -> Svg msg
translateBy vector =
    placeIn (Frame2d.translateBy vector Frame2d.xy)


{-| Mirror arbitrary SVG across a given axis.

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#mirrored" style="width: 230px; height: 280px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#mirrored>

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

    topLeftFrame =
        Frame2d
            { originPoint = Point2d ( 0, 300 )
            , xDirection = Direction2d.positiveX
            , yDirection = Direction2d.negativeY
            }

(As expressed in the model frame, the top-left SVG frame is at the point
(0, 300) and its Y direction is equal to the global negative Y direction.) If
`scene` is an SVG element representing your scene, you could then transform it
into top-left SVG window coordinates and render the result to HTML with

    Svg.svg [ Attributes.width "300", Attributes.height "300" ]
        [ Svg.relativeTo topLeftFrame scene ]

-}
relativeTo : Frame2d -> Svg msg -> Svg msg
relativeTo frame =
    placeIn (Frame2d.relativeTo frame Frame2d.xy)


{-| Take SVG defined in local coordinates relative to a given reference frame,
and return that SVG expressed in global coordinates.

This can be useful for taking a chunk of SVG and 'stamping' it in different
positions with different orientations:

<iframe src="https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#placed" style="width: 225px; height: 180px" scrolling=no frameborder=0></iframe>
<https://opensolid.github.io/images/svg/1.1/DocumentationExamples.html#placed>

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
            List.map toString [ x1, y1, x2, y2, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
        Svg.g [ Attributes.transform transform ] [ element ]
