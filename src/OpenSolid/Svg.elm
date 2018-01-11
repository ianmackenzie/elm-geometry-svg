module OpenSolid.Svg
    exposing
        ( arc2d
        , boundingBox2d
        , circle2d
        , cubicSpline2d
        , direction2d
        , direction2dWith
        , ellipse2d
        , ellipticalArc2d
        , lineSegment2d
        , mirrorAcross
        , placeIn
        , point2d
        , point2dWith
        , polygon2d
        , polyline2d
        , quadraticSpline2d
        , relativeTo
        , render2d
        , rotateAround
        , scaleAbout
        , text2d
        , translateBy
        , triangle2d
        , vector2d
        , vector2dWith
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

Also assume that any necessary OpenSolid modules/types have been imported using
the following format:

    import OpenSolid.Point2d as Point2d exposing (Point2d)

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

@docs vector2d, vector2dWith, direction2d, direction2dWith, point2d, point2dWith


# Geometry

These functions turn OpenSolid values into SVG elements with geometric
attributes such as `points` and `transform` set appropriately. Each function
also accepts a list of additional SVG attributes such as `fill` or `stroke` that
should be added to the resulting element.

@docs lineSegment2d, triangle2d, polyline2d, polygon2d, arc2d, ellipticalArc2d, circle2d, ellipse2d, quadraticSpline2d, cubicSpline2d, boundingBox2d


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

import Basics.Extra exposing (inDegrees)
import Html exposing (Html)
import Html.Attributes
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
import OpenSolid.Svg.Internal as Internal
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attributes


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
            Frame2d.atPoint (Point2d.fromCoordinates ( minX, maxY ))
                |> Frame2d.flipY

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox
    in
    Html.div
        [ Html.Attributes.style
            [ ( "border", "0" )
            , ( "padding", "0" )
            , ( "margin", "0" )
            , ( "display", "inline-block" )
            ]
        ]
        [ Svg.svg
            [ Attributes.width (toString width)
            , Attributes.height (toString height)
            , Html.Attributes.style [ ( "display", "block" ) ]
            ]
            [ relativeTo topLeftFrame svg ]
        ]


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


{-| Draw a vector with the given attributes, starting from the given base point.
Nothing will be drawn if the vector length is zero. Vectors are drawn as an
arrow with a stem line and a tip triangle:

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#vector" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#vector`
</iframe>

    vector : Svg Never
    vector =
        Svg.vector2d
            [ Attributes.fill "black"
            , Attributes.stroke "black"
            ]
            (Point2d.fromCoordinates ( 100, 100 ))
            (Vector2d.fromComponents ( 100, 100 ))

To customize the size of the tip, use `Svg.vector2dWith`; `Svg.vector2d` is
defined as

    Svg.vector2dWith { tipLength = 10, tipWidth = 8 }

-}
vector2d : List (Attribute msg) -> Point2d -> Vector2d -> Svg msg
vector2d =
    vector2dWith { tipLength = 10, tipWidth = 8 }


{-| Draw a vector with a custom tip length and width.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#customVector" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#customVector`
</iframe>

    customVector : Svg Never
    customVector =
        Svg.vector2dWith { tipLength = 30, tipWidth = 30 }
            [ Attributes.fill "orange"
            , Attributes.stroke "blue"
            , Attributes.strokeWidth "2"
            ]
            (Point2d.fromCoordinates ( 100, 100 ))
            (Vector2d.fromComponents ( 100, 100 ))

-}
vector2dWith : { tipLength : Float, tipWidth : Float } -> List (Attribute msg) -> Point2d -> Vector2d -> Svg msg
vector2dWith options attributes basePoint vector =
    case Vector2d.lengthAndDirection vector of
        Just ( length, direction ) ->
            let
                frame =
                    Frame2d.with
                        { originPoint = basePoint
                        , xDirection = direction
                        }

                tipBasePoint =
                    Point2d.in_ frame ( length - options.tipLength, 0 )

                stem =
                    LineSegment2d.from basePoint tipBasePoint

                tip =
                    Internal.tip options basePoint length direction
            in
            Svg.g attributes [ lineSegment2d [] stem, triangle2d [] tip ]

        Nothing ->
            Svg.text ""


{-| Draw a direction with the given attributes, starting from the given base
point.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#directions" style="width: 70px; height: 70px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#directions`
</iframe>

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

This function is similar to `Svg.vector2d`, but a default length is baked in as
well as a default tip length and width; `Svg.direction2d` is defined as

    Svg.direction2dWith
        { length = 50
        , tipLength = 9
        , tipWidth = 9
        }

-}
direction2d : List (Attribute msg) -> Point2d -> Direction2d -> Svg msg
direction2d =
    direction2dWith { length = 50, tipLength = 9, tipWidth = 9 }


{-| Draw a direction with a custom length, tip length and width.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#customDirections" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#customDirections`
</iframe>

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

-}
direction2dWith : { length : Float, tipLength : Float, tipWidth : Float } -> List (Attribute msg) -> Point2d -> Direction2d -> Svg msg
direction2dWith options attributes basePoint direction =
    vector2dWith
        { tipLength = options.tipLength
        , tipWidth = options.tipWidth
        }
        attributes
        basePoint
        (Vector2d.with { length = options.length, direction = direction })


{-| Draw a point as an SVG `<circle>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#points" style="width: 110px; height: 110px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#points`
</iframe>

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

Use `Svg.point2dWith` to customize the circle radius; `Svg.point2d` is defined
as

    Svg.point2dWith { radius = 3 }

-}
point2d : List (Attribute msg) -> Point2d -> Svg msg
point2d =
    point2dWith { radius = 3 }


{-| Draw a point with a custom radius.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#customPoints" style="width: 110px; height: 110px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#customPoints`
</iframe>

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

-}
point2dWith : { radius : Float } -> List (Attribute msg) -> Point2d -> Svg msg
point2dWith { radius } attributes point =
    circle2d attributes (Circle2d.with { centerPoint = point, radius = radius })


{-| Draw a `LineSegment2d` as an SVG `<polyline>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#lineSegment" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#lineSegment`
</iframe>

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

-}
lineSegment2d : List (Attribute msg) -> LineSegment2d -> Svg msg
lineSegment2d attributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment
    in
    Svg.polyline (pointsAttribute [ p1, p2 ] :: attributes) []


{-| Draw a `Triangle2d` as an SVG `<polygon>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#triangle" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#triangle`
</iframe>

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

-}
triangle2d : List (Attribute msg) -> Triangle2d -> Svg msg
triangle2d attributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
    Svg.polygon (pointsAttribute [ p1, p2, p3 ] :: attributes) []


{-| Draw a `Polyline2d` as an SVG `<polyline>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#polyline" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#polyline`
</iframe>

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

-}
polyline2d : List (Attribute msg) -> Polyline2d -> Svg msg
polyline2d attributes polyline =
    let
        vertices =
            Polyline2d.vertices polyline
    in
    Svg.polyline (pointsAttribute vertices :: attributes) []


{-| Draw a `Polygon2d` as an SVG `<polygon>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#polygon" style="width: 120px; height: 70px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#polygon`
</iframe>

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

-}
polygon2d : List (Attribute msg) -> Polygon2d -> Svg msg
polygon2d attributes polygon =
    let
        vertices =
            Polygon2d.vertices polygon
    in
    Svg.polygon (pointsAttribute vertices :: attributes) []


{-| Draw an `Arc2d` as an SVG `<path>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#arc" style="width: 100px; height: 110px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#arc`
</iframe>

    arc : Svg Never
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
                    Point2d.coordinates (Arc2d.pointOn arc t)
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


{-| Draw an `EllipticalArc2d` as an SVG `<path>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#ellipticalArc" style="width: 120px; height: 120px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#ellipticalArc`
</iframe>

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
            toString xRadius

        yRadiusString =
            toString yRadius

        xDirection =
            EllipticalArc2d.xDirection arc

        angleString =
            toString (Direction2d.angle xDirection |> inDegrees)

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
                    Point2d.coordinates (EllipticalArc2d.pointOn arc t)
            in
            [ "A"
            , xRadiusString
            , yRadiusString
            , angleString
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

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#circle" style="width: 40px; height: 40px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#circle`
</iframe>

    circle : Svg Never
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
            Attributes.cx (toString x)

        cy =
            Attributes.cy (toString y)

        r =
            Attributes.r (toString (Circle2d.radius circle))
    in
    Svg.circle (cx :: cy :: r :: attributes) []


{-| Draw an `Ellipse2d` as an SVG `<ellipse>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#ellipse" style="width: 140px; height: 100px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#ellipse`
</iframe>

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

-}
ellipse2d : List (Attribute msg) -> Ellipse2d -> Svg msg
ellipse2d attributes ellipse =
    let
        centerPoint =
            Ellipse2d.centerPoint ellipse

        ( x, y ) =
            Point2d.coordinates centerPoint

        cx =
            Attributes.cx (toString x)

        cy =
            Attributes.cy (toString y)

        rx =
            Attributes.rx (toString (Ellipse2d.xRadius ellipse))

        ry =
            Attributes.ry (toString (Ellipse2d.yRadius ellipse))

        angle =
            Direction2d.angle (Ellipse2d.xDirection ellipse)
    in
    Svg.ellipse (cx :: cy :: rx :: ry :: attributes) []
        |> rotateAround centerPoint angle


{-| Draw a quadratic spline as an SVG `<path>` with the given attributes.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#quadraticSpline" style="width: 130px; height: 130px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#quadraticSpline`
</iframe>

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
            , Svg.g [ Attributes.fill "white" ]
                (points
                    |> List.map
                        (Svg.point2d
                            { radius = 3
                            , attributes = []
                            }
                        )
                )
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

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#cubicSpline" style="width: 190px; height: 165px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#cubicSpline`
</iframe>

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
            , Svg.g [ Attributes.fill "white" ]
                (points
                    |> List.map
                        (Svg.point2d
                            { radius = 3
                            , attributes = []
                            }
                        )
                )
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


{-| Draw a bounding box as an SVG `<rect>` with the given attributes.
-}
boundingBox2d : List (Attribute msg) -> BoundingBox2d -> Svg msg
boundingBox2d attributes boundingBox =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema boundingBox

        x =
            Attributes.x (toString minX)

        y =
            Attributes.y (toString minY)

        width =
            Attributes.width (toString (maxX - minX))

        height =
            Attributes.height (toString (maxY - minY))
    in
    Svg.rect (x :: y :: width :: height :: attributes) []



--{-| Draw a generic curve as SVG with the given attributes. Arcs, lines and
--quadratic and cubic splines will be drawn using native SVG paths. Other curves
--will be converted to polylines using the given tolerance.
---}
--curve2d : Float -> List (Attribute msg) -> Curve2d -> Svg msg
--curve2d tolerance attributes =
--    Curve2d.match
--        |> Curve2d.isLineSegment (lineSegment2d attributes)
--        |> Curve2d.isArc (arc2d attributes)
--        |> Curve2d.isQuadraticSpline (quadraticSpline2d attributes)
--        |> Curve2d.isCubicSpline (cubicSpline2d attributes)
--        |> Curve2d.otherwise
--            (Curve2d.toPolyline tolerance >> polyline2d attributes)


{-| Draw a string of text with the given attributes at the given point. You can
use the SVG [`textAnchor`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-anchor)
and [`alignmentBaseline`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignment-baseline)
attributes to align the text relative to the given point as desired.

The wrinkle is that this function assumes that you are constructing your diagram
in a coordinate system where positive X is to the right and positive Y is up. As
a result, it will actually draw the text _upside down_, assuming that you will
eventually flip your entire diagram upside down again to convert it to the
Y-down coordinate system used by SVG (perhaps by using `render2d`).

Note that you can apply all the usual transformations to SVG text just like any
other SVG element!

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#text" style="width: 220px; height: 170px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#text`
</iframe>

    drawText : Point2d -> String -> String -> Svg Never
    drawText point anchor baseline =
        Svg.g []
            [ Svg.point2d
                { radius = 2
                , attributes =
                    [ Attributes.fill "orange" ]
                }
                point
            , Svg.text2d
                [ Attributes.textAnchor anchor
                , Attributes.alignmentBaseline baseline
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

-}
text2d : List (Attribute msg) -> Point2d -> String -> Svg msg
text2d attributes basePoint text =
    let
        ( x, y ) =
            Point2d.coordinates basePoint

        mirrorAxis =
            Axis2d.with
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

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#scaled" style="width: 160px; height: 160px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#scaled`
</iframe>

    scaled : Svg Never
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

            scaledCircle : Float -> Svg Never
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
            List.map toString [ scale, 0, 0, scale, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
    Svg.g [ Attributes.transform transform ] [ element ]


{-| Rotate arbitrary SVG around a given point by a given angle.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#rotated" style="width: 140px; height: 140px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#rotated`
</iframe>

    rotated : Svg Never
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

            rotatedCircle : Float -> Svg Never
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
            toString x

        yString =
            toString y

        angleString =
            toString (angle |> inDegrees)

        rotate =
            "rotate(" ++ angleString ++ " " ++ xString ++ " " ++ yString ++ ")"
    in
    Svg.g [ Attributes.transform rotate ] [ element ]


{-| Translate arbitrary SVG by a given displacement.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#translated" style="width: 128px; height: 230px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#translated`
</iframe>

    translated : Svg Never
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

        translate =
            "translate(" ++ toString x ++ " " ++ toString y ++ ")"
    in
    Svg.g [ Attributes.transform translate ] [ element ]


{-| Mirror arbitrary SVG across a given axis.

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#mirrored" style="width: 230px; height: 280px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#mirrored`
</iframe>

    mirrored : Svg Never
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

<iframe src="https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#placed" style="width: 225px; height: 180px" scrolling=no frameborder=0>
`https://opensolid.github.io/svg/3.0.0/doc/images/DocumentationExamples.html#placed`
</iframe>

    placed : Svg Never
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
                    (Polygon2d.fromVertices vertices)

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
            List.map toString [ x1, y1, x2, y2, px, py ]

        transform =
            "matrix(" ++ String.join " " components ++ ")"
    in
    Svg.g [ Attributes.transform transform ] [ element ]
