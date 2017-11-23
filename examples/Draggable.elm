module Draggable exposing (..)

import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Svg.Draggable as Draggable exposing (Draggable)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes


type alias Model =
    { point : Draggable Point2d
    , direction : Draggable Direction2d
    , vector : Draggable Vector2d
    , p0 : Draggable Point2d
    , p1 : Draggable Point2d
    , p2 : Draggable Point2d
    , p3 : Draggable Point2d
    }


type Msg
    = UpdatedPoint (Draggable Point2d)
    | UpdatedDirection (Draggable Direction2d)
    | UpdatedVector (Draggable Vector2d)
    | UpdatedP0 (Draggable Point2d)
    | UpdatedP1 (Draggable Point2d)
    | UpdatedP2 (Draggable Point2d)
    | UpdatedP3 (Draggable Point2d)


whiteFill : Svg.Attribute msg
whiteFill =
    Svg.Attributes.fill "white"


blackFill : Svg.Attribute msg
blackFill =
    Svg.Attributes.fill "black"


noFill : Svg.Attribute msg
noFill =
    Svg.Attributes.fill "none"


blackStroke : Svg.Attribute msg
blackStroke =
    Svg.Attributes.stroke "black"


dashed : Svg.Attribute msg
dashed =
    Svg.Attributes.strokeDasharray "5 5"


pointOptions : Svg.PointOptions Never
pointOptions =
    { radius = 5
    , attributes = [ whiteFill, blackStroke ]
    }


directionOptions : Svg.DirectionOptions Never
directionOptions =
    { length = 50
    , tipWidth = 10
    , tipLength = 10
    , stemAttributes = []
    , tipAttributes = [ whiteFill ]
    , groupAttributes = [ blackStroke ]
    }


vectorOptions : Svg.VectorOptions Never
vectorOptions =
    { tipWidth = 10
    , tipLength = 10
    , stemAttributes = []
    , tipAttributes = [ blackFill ]
    , groupAttributes = [ blackStroke ]
    }


draggablePoint : ( Float, Float ) -> Draggable Point2d
draggablePoint coordinates =
    Draggable.point2d pointOptions (Point2d.fromCoordinates coordinates)


init : Model
init =
    { point = draggablePoint ( 100, 100 )
    , direction =
        Draggable.direction2d directionOptions
            (Point2d.fromCoordinates ( 300, 300 ))
            (Direction2d.fromAngle (degrees 45))
    , vector =
        Draggable.vector2d vectorOptions
            (Point2d.fromCoordinates ( 500, 500 ))
            (Vector2d.fromComponents ( 0, -30 ))
    , p0 = draggablePoint ( 100, 500 )
    , p1 = draggablePoint ( 200, 400 )
    , p2 = draggablePoint ( 300, 500 )
    , p3 = draggablePoint ( 400, 400 )
    }


boundingBox : BoundingBox2d
boundingBox =
    BoundingBox2d.with
        { minX = 0
        , minY = 0
        , maxX = 600
        , maxY = 600
        }


view : Model -> Html Msg
view model =
    let
        p0 =
            Draggable.currentValue model.p0

        p1 =
            Draggable.currentValue model.p1

        p2 =
            Draggable.currentValue model.p2

        p3 =
            Draggable.currentValue model.p3
    in
    Svg.render2d boundingBox <|
        Svg.g []
            [ Draggable.render boundingBox model.point
                |> Svg.map UpdatedPoint
            , Draggable.render boundingBox model.direction
                |> Svg.map UpdatedDirection
            , Draggable.render boundingBox model.vector
                |> Svg.map UpdatedVector
            , Svg.cubicSpline2d [ blackStroke, noFill ] <|
                CubicSpline2d.fromControlPoints ( p0, p1, p2, p3 )
            , Svg.g [ blackStroke, dashed ]
                (List.map (Svg.lineSegment2d [] << LineSegment2d.fromEndpoints)
                    [ ( p0, p1 )
                    , ( p1, p2 )
                    , ( p2, p3 )
                    ]
                )
            , Draggable.render boundingBox model.p0
                |> Svg.map UpdatedP0
            , Draggable.render boundingBox model.p1
                |> Svg.map UpdatedP1
            , Draggable.render boundingBox model.p2
                |> Svg.map UpdatedP2
            , Draggable.render boundingBox model.p3
                |> Svg.map UpdatedP3
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatedPoint updatedPoint ->
            { model | point = updatedPoint }

        UpdatedDirection updatedDirection ->
            { model | direction = updatedDirection }

        UpdatedVector updatedVector ->
            { model | vector = updatedVector }

        UpdatedP0 updatedP0 ->
            { model | p0 = updatedP0 }

        UpdatedP1 updatedP1 ->
            { model | p1 = updatedP1 }

        UpdatedP2 updatedP2 ->
            { model | p2 = updatedP2 }

        UpdatedP3 updatedP3 ->
            { model | p3 = updatedP3 }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
