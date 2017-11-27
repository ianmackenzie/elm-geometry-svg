module Drag exposing (..)

import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Svg.Drag as Drag exposing (Drag)
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import Svg exposing (Svg)
import Svg.Attributes


type DragTarget
    = Triangle
    | Vertex0
    | Vertex1
    | Vertex2
    | Edge0
    | Edge1
    | Edge2


type Msg
    = DragMsg (Drag.Event DragTarget)


type alias Model =
    { dragState : Drag.State DragTarget
    , triangle : Triangle2d
    }


init : ( Model, Cmd Msg )
init =
    ( { dragState = Drag.init
      , triangle =
            Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 200, 200 )
                , Point2d.fromCoordinates ( 400, 200 )
                , Point2d.fromCoordinates ( 200, 400 )
                )
      }
    , Cmd.none
    )


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
        ( p0, p1, p2 ) =
            Triangle2d.vertices model.triangle

        edge0 =
            LineSegment2d.from p0 p1

        edge1 =
            LineSegment2d.from p1 p2

        edge2 =
            LineSegment2d.from p2 p0
    in
    Svg.render2d boundingBox <|
        Svg.g []
            [ Svg.triangle2d (triangleAttributes model) model.triangle
            , Svg.lineSegment2d (edgeAttributes Edge0 model) edge0
            , Svg.lineSegment2d (edgeAttributes Edge1 model) edge1
            , Svg.lineSegment2d (edgeAttributes Edge2 model) edge2
            , Svg.point2d (pointOptions Vertex0 model) p0
            , Svg.point2d (pointOptions Vertex1 model) p1
            , Svg.point2d (pointOptions Vertex2 model) p2
            , Svg.g []
                [ Drag.triangleHandle model.triangle
                    { target = Triangle
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Drag.lineSegmentHandle edge0
                    { target = Edge0
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Drag.lineSegmentHandle edge1
                    { target = Edge1
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Drag.lineSegmentHandle edge2
                    { target = Edge2
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Drag.pointHandle p0
                    { target = Vertex0
                    , radius = 10
                    , renderBounds = boundingBox
                    }
                , Drag.pointHandle p1
                    { target = Vertex1
                    , radius = 10
                    , renderBounds = boundingBox
                    }
                , Drag.pointHandle p2
                    { target = Vertex2
                    , radius = 10
                    , renderBounds = boundingBox
                    }
                ]
                |> Svg.map DragMsg
            ]


isActive : DragTarget -> Model -> Bool
isActive dragTarget model =
    Drag.isHovering dragTarget model.dragState
        || Drag.isDragging dragTarget model.dragState


triangleAttributes : Model -> List (Svg.Attribute Msg)
triangleAttributes model =
    let
        fillColor =
            if isActive Triangle model then
                "rgb(63, 127, 255)"
            else
                "white"
    in
    [ Svg.Attributes.fill fillColor, Svg.Attributes.stroke "none" ]


edgeAttributes : DragTarget -> Model -> List (Svg.Attribute Msg)
edgeAttributes dragTarget model =
    let
        color =
            if isActive dragTarget model then
                "rgb(63, 127, 255)"
            else
                "black"
    in
    [ Svg.Attributes.stroke color ]


pointOptions : DragTarget -> Model -> Svg.PointOptions Msg
pointOptions dragTarget model =
    let
        fillColor =
            if isActive dragTarget model then
                "rgb(63, 127, 255)"
            else
                "white"
    in
    { radius = 5
    , attributes =
        [ Svg.Attributes.fill fillColor, Svg.Attributes.stroke "black" ]
    }


performDrag : Drag DragTarget -> Model -> Model
performDrag drag model =
    let
        displacement =
            Drag.translation drag

        ( p0, p1, p2 ) =
            Triangle2d.vertices model.triangle

        translatePoint =
            Point2d.translateBy displacement

        updatedTriangle =
            case Drag.target drag of
                Triangle ->
                    Triangle2d.translateBy displacement model.triangle

                Vertex0 ->
                    Triangle2d.fromVertices ( translatePoint p0, p1, p2 )

                Vertex1 ->
                    Triangle2d.fromVertices ( p0, translatePoint p1, p2 )

                Vertex2 ->
                    Triangle2d.fromVertices ( p0, p1, translatePoint p2 )

                Edge0 ->
                    Triangle2d.fromVertices
                        ( translatePoint p0
                        , translatePoint p1
                        , p2
                        )

                Edge1 ->
                    Triangle2d.fromVertices
                        ( p0
                        , translatePoint p1
                        , translatePoint p2
                        )

                Edge2 ->
                    Triangle2d.fromVertices
                        ( translatePoint p0
                        , p1
                        , translatePoint p2
                        )
    in
    { model | triangle = updatedTriangle }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        DragMsg dragEvent ->
            Drag.apply performDrag dragEvent model


subscriptions : Model -> Sub Msg
subscriptions model =
    Drag.subscriptions model.dragState |> Sub.map DragMsg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
