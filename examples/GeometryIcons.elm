module GeometryIcons exposing (..)

import Html exposing (Html)
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Arc3d as Arc3d exposing (Arc3d)
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Circle3d as Circle3d exposing (Circle3d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.CubicSpline3d as CubicSpline3d exposing (CubicSpline3d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Ellipse2d as Ellipse2d exposing (Ellipse2d)
import OpenSolid.EllipticalArc2d as EllipticalArc2d exposing (EllipticalArc2d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Interval as Interval exposing (Interval)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d exposing (QuadraticSpline3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Svg as Svg
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Triangle3d as Triangle3d exposing (Triangle3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


viewPlane : SketchPlane3d
viewPlane =
    Frame3d.xyz
        |> Frame3d.rotateAroundOwn Frame3d.zAxis (degrees 30)
        |> Frame3d.rotateAroundOwn Frame3d.yAxis (degrees -30)
        |> Frame3d.yzSketchPlane


vector2d : Point2d -> Vector2d -> Svg Never
vector2d =
    Svg.vector2dWith { tipLength = 6, tipWidth = 5 } [ Attributes.fill "black" ]


vector3d : Point3d -> Vector3d -> Svg Never
vector3d point vector =
    vector2d (Point3d.projectInto viewPlane point)
        (Vector3d.projectInto viewPlane vector)


direction2d : Point2d -> Direction2d -> Svg Never
direction2d =
    Svg.direction2dWith { length = 25, tipLength = 5, tipWidth = 5 }
        [ Attributes.fill "white" ]


direction3d : Point3d -> Direction3d -> Svg Never
direction3d point direction =
    let
        vector =
            Vector3d.with { length = 25, direction = direction }
    in
    Svg.vector2dWith { tipLength = 5, tipWidth = 5 }
        [ Attributes.fill "white" ]
        (Point3d.projectInto viewPlane point)
        (Vector3d.projectInto viewPlane vector)


originPoint2d : Point2d -> Svg Never
originPoint2d =
    Svg.point2dWith { radius = 1.5 } [ Attributes.fill "white" ]


originPoint3d : Point3d -> Svg Never
originPoint3d =
    Point3d.projectInto viewPlane >> originPoint2d


frame2d : Frame2d -> Svg Never
frame2d frame =
    let
        originPoint =
            Frame2d.originPoint frame
    in
    Svg.g []
        [ direction2d originPoint (Frame2d.xDirection frame)
        , direction2d originPoint (Frame2d.yDirection frame)
        , originPoint2d originPoint
        ]


frame3d : Frame3d -> Svg Never
frame3d frame =
    let
        originPoint =
            Frame3d.originPoint frame
    in
    Svg.g []
        [ direction3d originPoint (Frame3d.xDirection frame)
        , direction3d originPoint (Frame3d.yDirection frame)
        , direction3d originPoint (Frame3d.zDirection frame)
        , originPoint3d originPoint
        ]


indicator2d : Svg Never
indicator2d =
    Svg.g [ Attributes.stroke "darkgrey" ] [ frame2d Frame2d.xy ]


indicator3d : Svg Never
indicator3d =
    Svg.g [ Attributes.stroke "darkgrey" ] [ frame3d Frame3d.xyz ]


point2d : Point2d -> Svg Never
point2d point =
    let
        circle =
            Circle2d.with { centerPoint = point, radius = 2 }

        localPoint coordinates =
            Point2d.in_ (Frame2d.atPoint point) coordinates

        horizontal =
            LineSegment2d.fromEndpoints
                ( localPoint ( -6, 0 )
                , localPoint ( 6, 0 )
                )

        vertical =
            LineSegment2d.fromEndpoints
                ( localPoint ( 0, -6 )
                , localPoint ( 0, 6 )
                )
    in
    Svg.g []
        [ Svg.lineSegment2d [] horizontal
        , Svg.lineSegment2d [] vertical
        , Svg.circle2d [] circle
        ]


point3d : Point3d -> Svg Never
point3d =
    Point3d.projectInto viewPlane >> point2d


axis2d : Interval -> Axis2d -> Svg Never
axis2d interval axis =
    let
        originPoint =
            Axis2d.originPoint axis

        { minValue, maxValue } =
            Interval.extrema interval

        segment =
            LineSegment2d.fromEndpoints
                ( Point2d.along axis minValue
                , Point2d.along axis maxValue
                )
    in
    Svg.lineSegment2d
        [ Attributes.strokeDasharray "3 3"
        , Attributes.strokeWidth "0.75"
        ]
        segment


axis3d : Interval -> Axis3d -> Svg Never
axis3d interval axis =
    let
        originPoint =
            Axis3d.originPoint axis

        { minValue, maxValue } =
            Interval.extrema interval

        segment =
            LineSegment3d.fromEndpoints
                ( Point3d.along axis minValue
                , Point3d.along axis maxValue
                )
                |> LineSegment3d.projectInto viewPlane
    in
    Svg.lineSegment2d
        [ Attributes.strokeDasharray "3 3"
        , Attributes.strokeWidth "0.75"
        ]
        segment


plane3d : SketchPlane3d -> Svg Never
plane3d sketchPlane =
    let
        point x y =
            Point2d.fromCoordinates ( x, y )
                |> Point3d.on sketchPlane
                |> Point3d.projectInto viewPlane

        outline =
            Polygon2d.fromVertices
                [ point -16 -16
                , point 16 -16
                , point 16 16
                , point -16 16
                ]

        originPoint =
            SketchPlane3d.originPoint sketchPlane

        normalDirection =
            SketchPlane3d.normalDirection sketchPlane
    in
    Svg.g []
        [ Svg.polygon2d
            [ Attributes.fill "none"
            , Attributes.strokeDasharray "3 3"
            , Attributes.strokeWidth "0.75"
            ]
            outline
        , direction3d originPoint normalDirection
        , originPoint3d originPoint
        ]


sketchPlane3d : SketchPlane3d -> Svg Never
sketchPlane3d sketchPlane =
    let
        point x y =
            Point2d.fromCoordinates ( x, y )
                |> Point3d.on sketchPlane
                |> Point3d.projectInto viewPlane

        outline =
            Polygon2d.fromVertices
                [ point -6 -6
                , point 30 -6
                , point 30 30
                , point -6 30
                ]

        originPoint =
            SketchPlane3d.originPoint sketchPlane
    in
    Svg.g []
        [ Svg.polygon2d
            [ Attributes.fill "none"
            , Attributes.strokeDasharray "3 3"
            , Attributes.strokeWidth "0.75"
            ]
            outline
        , direction3d originPoint (SketchPlane3d.xDirection sketchPlane)
        , direction3d originPoint (SketchPlane3d.yDirection sketchPlane)
        , originPoint3d originPoint
        ]


icon2d : Svg Never -> Svg Never
icon2d svg =
    Svg.g [] [ indicator2d, svg ]


icon3d : Svg Never -> Svg Never
icon3d svg =
    Svg.g [] [ indicator3d, svg ]


point2dIcon : Svg Never
point2dIcon =
    icon2d (point2d (Point2d.fromCoordinates ( 30, 30 )))


point3dIcon : Svg Never
point3dIcon =
    icon3d (point3d (Point3d.fromCoordinates ( 0, 40, 30 )))


vector2dIcon : Svg Never
vector2dIcon =
    icon2d (vector2d Point2d.origin (Vector2d.fromComponents ( 30, 30 )))


vector3dIcon : Svg Never
vector3dIcon =
    icon3d (vector3d Point3d.origin (Vector3d.fromComponents ( 0, 40, 30 )))


direction2dIcon : Svg Never
direction2dIcon =
    icon2d <| direction2d Point2d.origin (Direction2d.fromAngle (degrees 45))


direction3dIcon : Svg Never
direction3dIcon =
    icon3d <|
        direction3d Point3d.origin
            (Direction3d.with { azimuth = degrees 90, elevation = degrees 35 })


axis2dIcon : Svg Never
axis2dIcon =
    let
        originPoint =
            Point2d.fromCoordinates ( 10, 15 )

        direction =
            Direction2d.fromAngle (degrees 20)

        axis =
            Axis2d.with
                { originPoint = originPoint
                , direction = direction
                }

        interval =
            Interval.with { minValue = -20, maxValue = 45 }
    in
    Svg.g []
        [ axis2d interval axis
        , direction2d originPoint direction
        , originPoint2d originPoint
        ]
        |> icon2d


axis3dIcon : Svg Never
axis3dIcon =
    let
        originPoint =
            Point3d.fromCoordinates ( 0, 20, 40 )

        direction =
            Direction3d.on SketchPlane3d.yz <|
                Direction2d.fromAngle (degrees -20)

        axis =
            Axis3d.with
                { originPoint = originPoint
                , direction = direction
                }

        interval =
            Interval.with { minValue = -20, maxValue = 45 }
    in
    Svg.g []
        [ axis3d interval axis
        , direction3d originPoint (Axis3d.direction axis)
        , originPoint3d originPoint
        ]
        |> icon3d


plane3dIcon : Svg Never
plane3dIcon =
    let
        sketchPlane =
            SketchPlane3d.xy
                |> SketchPlane3d.rotateAround Axis3d.x (degrees -20)
                |> SketchPlane3d.rotateAround Axis3d.y (degrees -5)
                |> SketchPlane3d.moveTo (Point3d.fromCoordinates ( 0, 40, 30 ))
    in
    icon3d (plane3d sketchPlane)


sketchPlane3dIcon : Svg Never
sketchPlane3dIcon =
    let
        sketchPlane =
            SketchPlane3d.xy
                |> SketchPlane3d.rotateAround Axis3d.x (degrees -10)
                |> SketchPlane3d.moveTo (Point3d.fromCoordinates ( 0, 32, 38 ))
    in
    icon3d (sketchPlane3d sketchPlane)


frame2dIcon : Svg Never
frame2dIcon =
    let
        frame =
            Frame2d.atPoint (Point2d.fromCoordinates ( 25, 15 ))
                |> Frame2d.rotateBy (degrees 20)
    in
    icon2d (frame2d frame)


frame3dIcon : Svg Never
frame3dIcon =
    let
        frame =
            Frame3d.atPoint (Point3d.fromCoordinates ( 0, 30, 30 ))
                |> Frame3d.rotateAroundOwn Frame3d.zAxis (degrees 20)
                |> Frame3d.rotateAroundOwn Frame3d.xAxis (degrees 15)
    in
    icon3d (frame3d frame)


vertex2d : Point2d -> Svg Never
vertex2d point =
    Svg.circle2d [] (Circle2d.with { centerPoint = point, radius = 1 })


vertex3d : Point3d -> Svg Never
vertex3d =
    Point3d.projectInto viewPlane >> vertex2d


polyline2d : Polyline2d -> Svg Never
polyline2d polyline =
    Svg.g []
        [ Svg.polyline2d [ Attributes.fill "none" ] polyline
        , Svg.g [] (List.map vertex2d (Polyline2d.vertices polyline))
        ]


polyline3d : Polyline3d -> Svg Never
polyline3d =
    Polyline3d.projectInto viewPlane >> polyline2d


polygon2d : Polygon2d -> Svg Never
polygon2d polygon =
    Svg.g []
        [ Svg.polygon2d [ Attributes.fill "none" ] polygon
        , Svg.g [] (List.map vertex2d (Polygon2d.vertices polygon))
        ]


lineSegment2d : LineSegment2d -> Svg Never
lineSegment2d lineSegment =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment
    in
    polyline2d (Polyline2d.fromVertices [ p1, p2 ])


lineSegment3d : LineSegment3d -> Svg Never
lineSegment3d =
    LineSegment3d.projectInto viewPlane >> lineSegment2d


triangle2d : Triangle2d -> Svg Never
triangle2d triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
    polygon2d (Polygon2d.fromVertices [ p1, p2, p3 ])


triangle3d : Triangle3d -> Svg Never
triangle3d =
    Triangle3d.projectInto viewPlane >> triangle2d


lineSegment2dIcon : Svg Never
lineSegment2dIcon =
    let
        lineSegment =
            LineSegment2d.fromEndpoints
                ( Point2d.fromCoordinates ( 15, 20 )
                , Point2d.fromCoordinates ( 40, 30 )
                )
    in
    icon2d (lineSegment2d lineSegment)


lineSegment3dIcon : Svg Never
lineSegment3dIcon =
    let
        lineSegment =
            LineSegment3d.fromEndpoints
                ( Point3d.fromCoordinates ( 0, 15, 20 )
                , Point3d.fromCoordinates ( 0, 50, 40 )
                )
    in
    icon3d (lineSegment3d lineSegment)


triangle2dIcon : Svg Never
triangle2dIcon =
    let
        triangle =
            Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 10, 10 )
                , Point2d.fromCoordinates ( 35, 15 )
                , Point2d.fromCoordinates ( 20, 30 )
                )
    in
    icon2d (triangle2d triangle)


triangle3dIcon : Svg Never
triangle3dIcon =
    let
        triangle =
            Triangle3d.fromVertices
                ( Point3d.fromCoordinates ( 0, 15, 20 )
                , Point3d.fromCoordinates ( 0, 40, 20 )
                , Point3d.fromCoordinates ( -20, 25, 35 )
                )
    in
    icon3d (triangle3d triangle)


boundingBox2d : BoundingBox2d -> Svg Never
boundingBox2d boundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox

        vertices =
            [ Point2d.fromCoordinates ( minX, minY )
            , Point2d.fromCoordinates ( maxX, minY )
            , Point2d.fromCoordinates ( maxX, maxY )
            , Point2d.fromCoordinates ( minX, maxY )
            ]
    in
    Svg.g []
        [ Svg.polygon2d [ Attributes.fill "none" ]
            (Polygon2d.fromVertices vertices)
        , Svg.g [] (List.map vertex2d vertices)
        ]


boundingBox3d : BoundingBox3d -> Svg Never
boundingBox3d boundingBox =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema boundingBox

        projected x y z =
            Point3d.fromCoordinates ( x, y, z )
                |> Point3d.projectInto viewPlane

        p0 =
            projected minX minY minZ

        p1 =
            projected maxX minY minZ

        p2 =
            projected maxX maxY minZ

        p3 =
            projected minX maxY minZ

        p4 =
            projected minX minY maxZ

        p5 =
            projected maxX minY maxZ

        p6 =
            projected maxX maxY maxZ

        p7 =
            projected minX maxY maxZ

        vertices =
            [ p0, p1, p2, p3, p4, p5, p6, p7 ]

        edges =
            List.map LineSegment2d.fromEndpoints
                [ ( p0, p1 )
                , ( p1, p2 )
                , ( p2, p3 )
                , ( p3, p0 )
                , ( p4, p5 )
                , ( p5, p6 )
                , ( p6, p7 )
                , ( p7, p4 )
                , ( p0, p4 )
                , ( p1, p5 )
                , ( p2, p6 )
                , ( p3, p7 )
                ]
    in
    Svg.g []
        [ Svg.g [] (List.map (Svg.lineSegment2d []) edges)
        , Svg.g [] (List.map vertex2d vertices)
        ]


boundingBox2dIcon : Svg Never
boundingBox2dIcon =
    let
        boundingBox =
            BoundingBox2d.with
                { minX = 15
                , maxX = 45
                , minY = 10
                , maxY = 30
                }
    in
    icon2d (boundingBox2d boundingBox)


boundingBox3dIcon : Svg Never
boundingBox3dIcon =
    let
        boundingBox =
            BoundingBox3d.with
                { minX = -20
                , maxX = 15
                , minY = 20
                , maxY = 45
                , minZ = 25
                , maxZ = 40
                }
    in
    icon3d (boundingBox3d boundingBox)


circle3d : Circle3d -> Svg Never
circle3d =
    Circle3d.projectInto viewPlane >> ellipse2d


circle2dIcon : Svg Never
circle2dIcon =
    let
        centerPoint =
            Point2d.fromCoordinates ( 30, 30 )

        circle =
            Circle2d.with { centerPoint = centerPoint, radius = 15 }
    in
    Svg.g []
        [ Svg.circle2d [ Attributes.fill "none" ] circle
        , point2d centerPoint
        ]
        |> icon2d


circle3dIcon : Svg Never
circle3dIcon =
    let
        centerPoint =
            Point3d.fromCoordinates ( 0, 30, 30 )

        axialDirection =
            Direction3d.on SketchPlane3d.yz <|
                Direction2d.fromAngle (degrees 70)

        circle =
            Circle3d.with
                { centerPoint = centerPoint
                , axialDirection = axialDirection
                , radius = 15
                }
    in
    Svg.g []
        [ circle3d circle
        , point3d centerPoint
        , direction3d centerPoint axialDirection
        ]
        |> icon3d


ellipse2d : Ellipse2d -> Svg Never
ellipse2d =
    Svg.ellipse2d [ Attributes.fill "none" ]


ellipse2dIcon : Svg Never
ellipse2dIcon =
    let
        centerPoint =
            Point2d.fromCoordinates ( 25, 30 )

        xRadius =
            24

        yRadius =
            12

        ellipse =
            Ellipse2d.with
                { centerPoint = centerPoint
                , xDirection = Direction2d.fromAngle (degrees 50)
                , xRadius = 24
                , yRadius = 12
                }

        xInterval =
            Interval.with { minValue = -xRadius - 5, maxValue = xRadius + 5 }

        yInterval =
            Interval.with { minValue = -yRadius - 5, maxValue = yRadius + 5 }
    in
    Svg.g []
        [ axis2d xInterval (Ellipse2d.xAxis ellipse)
        , axis2d yInterval (Ellipse2d.yAxis ellipse)
        , point2d centerPoint
        , ellipse2d ellipse
        ]
        |> icon2d


arc2d : Arc2d -> Svg Never
arc2d arc =
    Svg.g []
        [ Svg.arc2d [ Attributes.fill "none" ] arc
        , vertex2d (Arc2d.startPoint arc)
        , vertex2d (Arc2d.endPoint arc)
        , point2d (Arc2d.centerPoint arc)
        ]


arc2dIcon : Svg Never
arc2dIcon =
    let
        arc =
            Arc2d.with
                { startPoint = Point2d.fromCoordinates ( 30, 10 )
                , centerPoint = Point2d.fromCoordinates ( 15, 15 )
                , sweptAngle = degrees 120
                }
    in
    icon2d (arc2d arc)


arc3d : Arc3d -> Svg Never
arc3d =
    Arc3d.projectInto viewPlane >> ellipticalArc2d


arc3dIcon : Svg Never
arc3dIcon =
    let
        startPoint =
            Point3d.fromCoordinates ( 0, 35, 10 )

        axialDirection =
            Direction3d.on SketchPlane3d.yz <|
                Direction2d.fromAngle (degrees 60)

        axis =
            Axis3d.with
                { originPoint = Point3d.fromCoordinates ( 0, 20, 15 )
                , direction = axialDirection
                }

        arc =
            Arc3d.around axis
                { startPoint = startPoint
                , sweptAngle = degrees 150
                }

        centerPoint =
            Arc3d.centerPoint arc
    in
    Svg.g []
        [ arc3d arc
        , vertex3d (Arc3d.startPoint arc)
        , vertex3d (Arc3d.endPoint arc)
        , point3d centerPoint
        , direction3d centerPoint axialDirection
        ]
        |> icon3d


ellipticalArc2d : EllipticalArc2d -> Svg Never
ellipticalArc2d =
    Svg.ellipticalArc2d [ Attributes.fill "none" ]


ellipticalArc2dIcon : Svg Never
ellipticalArc2dIcon =
    let
        centerPoint =
            Point2d.fromCoordinates ( 15, 20 )

        arc =
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = Direction2d.fromAngle (degrees 20)
                , startAngle = degrees -90
                , sweptAngle = degrees 180
                , xRadius = 24
                , yRadius = 12
                }
    in
    Svg.g []
        [ ellipticalArc2d arc
        , vertex2d (EllipticalArc2d.startPoint arc)
        , vertex2d (EllipticalArc2d.endPoint arc)
        , point2d centerPoint
        ]
        |> icon2d


polyline2dIcon : Svg Never
polyline2dIcon =
    let
        polyline =
            Polyline2d.fromVertices
                [ Point2d.fromCoordinates ( 10, 10 )
                , Point2d.fromCoordinates ( 20, 25 )
                , Point2d.fromCoordinates ( 30, 20 )
                , Point2d.fromCoordinates ( 40, 35 )
                , Point2d.fromCoordinates ( 30, 40 )
                ]
    in
    icon2d (polyline2d polyline)


polyline3dIcon : Svg Never
polyline3dIcon =
    let
        polyline =
            Polyline3d.fromVertices
                [ Point3d.fromCoordinates ( -15, 0, 30 )
                , Point3d.fromCoordinates ( -20, 15, 30 )
                , Point3d.fromCoordinates ( 0, 20, 30 )
                , Point3d.fromCoordinates ( 0, 30, 20 )
                , Point3d.fromCoordinates ( -20, 40, 35 )
                ]
    in
    icon3d (polyline3d polyline)


polygon2dIcon : Svg Never
polygon2dIcon =
    let
        displacement =
            Vector2d.fromComponents ( 25, 25 )

        polygon =
            List.range 0 4
                |> List.map (\n -> turns (toFloat n / 5))
                |> List.map
                    (\angle -> Point2d.fromPolarCoordinates ( 15, angle ))
                |> List.map (Point2d.translateBy displacement)
                |> Polygon2d.fromVertices
    in
    icon2d (polygon2d polygon)


quadraticSpline2d : QuadraticSpline2d -> Svg Never
quadraticSpline2d spline =
    let
        ( p1, p2, p3 ) =
            QuadraticSpline2d.controlPoints spline

        points =
            [ p1, p2, p3 ]
    in
    Svg.g []
        [ Svg.quadraticSpline2d
            [ Attributes.fill "none" ]
            spline
        , Svg.polyline2d
            [ Attributes.fill "none"
            , Attributes.strokeDasharray "2 2"
            ]
            (Polyline2d.fromVertices points)
        , Svg.g [ Attributes.fill "white" ]
            (List.map (Svg.point2dWith { radius = 2 } []) points)
        ]


cubicSpline2d : CubicSpline2d -> Svg Never
cubicSpline2d spline =
    let
        ( p1, p2, p3, p4 ) =
            CubicSpline2d.controlPoints spline

        points =
            [ p1, p2, p3, p4 ]
    in
    Svg.g []
        [ Svg.cubicSpline2d
            [ Attributes.fill "none" ]
            spline
        , Svg.polyline2d
            [ Attributes.fill "none"
            , Attributes.strokeDasharray "2 2"
            ]
            (Polyline2d.fromVertices points)
        , Svg.g [ Attributes.fill "white" ]
            (List.map (Svg.point2dWith { radius = 2 } []) points)
        ]


quadraticSpline3d : QuadraticSpline3d -> Svg Never
quadraticSpline3d spline =
    let
        ( p1, p2, p3 ) =
            QuadraticSpline3d.controlPoints spline

        points =
            [ p1, p2, p3 ]

        drawControlPoint point =
            Svg.point2dWith { radius = 2 } [] <|
                Point3d.projectInto viewPlane point
    in
    Svg.g []
        [ Svg.quadraticSpline2d
            [ Attributes.fill "none" ]
            (QuadraticSpline3d.projectInto viewPlane spline)
        , Svg.polyline2d
            [ Attributes.fill "none"
            , Attributes.strokeDasharray "2 2"
            ]
            (Polyline3d.fromVertices points |> Polyline3d.projectInto viewPlane)
        , Svg.g [ Attributes.fill "white" ]
            (List.map drawControlPoint points)
        ]


cubicSpline3d : CubicSpline3d -> Svg Never
cubicSpline3d spline =
    let
        ( p1, p2, p3, p4 ) =
            CubicSpline3d.controlPoints spline

        points =
            [ p1, p2, p3, p4 ]

        drawControlPoint point =
            Svg.point2dWith { radius = 2 } [] <|
                Point3d.projectInto viewPlane point
    in
    Svg.g []
        [ Svg.cubicSpline2d
            [ Attributes.fill "none" ]
            (CubicSpline3d.projectInto viewPlane spline)
        , Svg.polyline2d
            [ Attributes.fill "none"
            , Attributes.strokeDasharray "2 2"
            ]
            (Polyline3d.fromVertices points |> Polyline3d.projectInto viewPlane)
        , Svg.g [ Attributes.fill "white" ]
            (List.map drawControlPoint points)
        ]


quadraticSpline2dIcon : Svg Never
quadraticSpline2dIcon =
    let
        spline =
            QuadraticSpline2d.fromControlPoints
                ( Point2d.fromCoordinates ( 10, 10 )
                , Point2d.fromCoordinates ( 20, 30 )
                , Point2d.fromCoordinates ( 40, 20 )
                )
    in
    icon2d (quadraticSpline2d spline)


cubicSpline2dIcon : Svg Never
cubicSpline2dIcon =
    let
        spline =
            CubicSpline2d.fromControlPoints
                ( Point2d.fromCoordinates ( 10, 10 )
                , Point2d.fromCoordinates ( 15, 30 )
                , Point2d.fromCoordinates ( 30, 15 )
                , Point2d.fromCoordinates ( 40, 35 )
                )
    in
    icon2d (cubicSpline2d spline)


quadraticSpline3dIcon : Svg Never
quadraticSpline3dIcon =
    let
        spline =
            QuadraticSpline3d.fromControlPoints
                ( Point3d.fromCoordinates ( 0, 10, 10 )
                , Point3d.fromCoordinates ( 20, 35, 55 )
                , Point3d.fromCoordinates ( 0, 40, 25 )
                )
    in
    icon3d (quadraticSpline3d spline)


cubicSpline3dIcon : Svg Never
cubicSpline3dIcon =
    let
        spline =
            CubicSpline3d.fromControlPoints
                ( Point3d.fromCoordinates ( 0, 15, 45 )
                , Point3d.fromCoordinates ( 0, 10, 10 )
                , Point3d.fromCoordinates ( 0, 35, 40 )
                , Point3d.fromCoordinates ( 0, 35, 15 )
                )
    in
    icon3d (cubicSpline3d spline)


sphere3dIcon : Svg Never
sphere3dIcon =
    let
        centerPoint =
            Point2d.fromCoordinates ( 25, 25 )

        circle =
            Circle2d.with { centerPoint = centerPoint, radius = 15 }
    in
    Svg.g []
        [ Svg.circle2d [ Attributes.fill "none" ] circle
        , point2d centerPoint
        ]
        |> icon3d


icons : List (Svg Never)
icons =
    [ point2dIcon
    , point3dIcon
    , vector2dIcon
    , vector3dIcon
    , direction2dIcon
    , direction3dIcon
    , axis2dIcon
    , axis3dIcon
    , plane3dIcon
    , sketchPlane3dIcon
    , frame2dIcon
    , frame3dIcon
    , lineSegment2dIcon
    , lineSegment3dIcon
    , triangle2dIcon
    , triangle3dIcon
    , boundingBox2dIcon
    , boundingBox3dIcon
    , circle2dIcon
    , circle3dIcon
    , ellipse2dIcon
    , arc2dIcon
    , arc3dIcon
    , ellipticalArc2dIcon
    , polyline2dIcon
    , polyline3dIcon
    , polygon2dIcon
    , quadraticSpline2dIcon
    , cubicSpline2dIcon
    , quadraticSpline3dIcon
    , cubicSpline3dIcon
    , sphere3dIcon
    ]


topLeftFrame : Frame2d
topLeftFrame =
    Frame2d.atPoint (Point2d.fromCoordinates ( -20, 60 ))
        |> Frame2d.flipY


container : Svg Never -> Html Never
container svg =
    Html.div []
        [ Svg.svg
            [ Attributes.width "80"
            , Attributes.height "80"
            , Attributes.stroke "black"
            ]
            [ Svg.relativeTo topLeftFrame svg
            , Svg.polygon2d
                [ Attributes.strokeWidth "1"
                , Attributes.stroke "none"
                , Attributes.fill "none"
                ]
                (Polygon2d.fromVertices
                    [ Point2d.fromCoordinates ( 0, 0 )
                    , Point2d.fromCoordinates ( 80, 0 )
                    , Point2d.fromCoordinates ( 80, 80 )
                    , Point2d.fromCoordinates ( 0, 80 )
                    ]
                )
            ]
        ]


divider : Html Never
divider =
    Html.hr [] []


main : Html Never
main =
    Html.div [] (List.map container icons)
