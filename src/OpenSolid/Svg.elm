module OpenSolid.Svg
    exposing
        ( lineSegment2d
        , triangle2d
        , polyline2d
        , polygon2d
        , circle2d
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
coordinate system; they were all rendered with a final
<code>Svg.relativeTo&nbsp;topLeftFrame</code> call as described in the
[relativeTo](#relativeTo) documentation.


# Geometry

These functions turn OpenSolid values into SVG elements with geometric
attributes such as `points` and `transform` set appropriately. Each function
also accepts a list of additional SVG attributes such as `fill` or `stroke` that
should be added to the resulting element.

@docs lineSegment2d, triangle2d, polyline2d, polygon2d, circle2d


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

import Svg as Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Polyline2d as Polyline2d
import OpenSolid.Polygon2d as Polygon2d
import OpenSolid.Circle2d as Circle2d


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


{-| Draw a `LineSegment2d` as an SVG `<polyline>` with the given attributes.

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

![lineSegment2d](https://opensolid.github.io/images/svg/1.0/lineSegment2d.svg)

-}
lineSegment2d : List (Attribute msg) -> LineSegment2d -> Svg msg
lineSegment2d attributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment
    in
        Svg.polyline (pointsAttribute [ p1, p2 ] :: attributes) []


{-| Draw a `Triangle2d` as an SVG `<polygon>` with the given attributes.

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

![triangle2d](https://opensolid.github.io/images/svg/1.0/triangle2d.svg)

-}
triangle2d : List (Attribute msg) -> Triangle2d -> Svg msg
triangle2d attributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
        Svg.polygon (pointsAttribute [ p1, p2, p3 ] :: attributes) []


{-| Draw a `Polyline2d` as an SVG `<polyline>` with the given attributes.

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

![polyline2d](https://opensolid.github.io/images/svg/1.0/polyline2d.svg)

-}
polyline2d : List (Attribute msg) -> Polyline2d -> Svg msg
polyline2d attributes polyline =
    let
        vertices =
            Polyline2d.vertices polyline
    in
        Svg.polyline (pointsAttribute vertices :: attributes) []


{-| Draw a `Polygon2d` as an SVG `<polygon>` with the given attributes.

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

![polygon2d](https://opensolid.github.io/images/svg/1.0/polygon2d.svg)

-}
polygon2d : List (Attribute msg) -> Polygon2d -> Svg msg
polygon2d attributes polygon =
    let
        vertices =
            Polygon2d.vertices polygon
    in
        Svg.polygon (pointsAttribute vertices :: attributes) []


{-| Draw a `Circle2d` as an SVG `<circle>` with the given attributes.

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

![circle2d](https://opensolid.github.io/images/svg/1.0/circle2d.svg)

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


{-| Scale arbitrary SVG around a given point by a given scale.

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

![scaleAbout](https://opensolid.github.io/images/svg/1.0.2/scaleAbout.svg)

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

![rotateAround](https://opensolid.github.io/images/svg/1.0.2/rotateAround.svg)

-}
rotateAround : Point2d -> Float -> Svg msg -> Svg msg
rotateAround point angle =
    placeIn (Frame2d.rotateAround point angle Frame2d.xy)


{-| Translate arbitrary SVG by a given displacement.

    translatedSvg : Svg Never
    translatedSvg =
        Svg.g []
            [ polylineSvg
            , Svg.translateBy (Vector2d ( 0, 40 )) polylineSvg
            , Svg.translateBy (Vector2d ( 5, -60 )) polylineSvg
            ]

![translateBy](https://opensolid.github.io/images/svg/1.0/translateBy.svg)

-}
translateBy : Vector2d -> Svg msg -> Svg msg
translateBy vector =
    placeIn (Frame2d.translateBy vector Frame2d.xy)


{-| Mirror arbitrary SVG across a given axis.

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

![mirrorAcross](https://opensolid.github.io/images/svg/1.0/mirrorAcross.svg)

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
            , xDirection = Direction2d.x
            , yDirection = Direction2d.flip Direction2d.y
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

![placeIn](https://opensolid.github.io/images/svg/1.0/placeIn.svg)

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
