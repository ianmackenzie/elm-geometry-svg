module Drag exposing (..)

import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
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


type Msg
    = DragMsg (Drag.Event DragTarget)


type alias Model =
    { dragState : Drag.State DragTarget
    , triangle : Triangle2d
    }


init : ( Model, Cmd Msg )
init =
    ( { dragState = Drag.none
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
    in
    Svg.render2d boundingBox <|
        Svg.g []
            [ Svg.triangle2d (attributes Triangle model) model.triangle
            , Svg.point2d (pointOptions Vertex0 model) p0
            , Svg.point2d (pointOptions Vertex1 model) p1
            , Svg.point2d (pointOptions Vertex2 model) p2
            , Svg.g []
                [ Drag.triangleHandle model.triangle
                    { target = Triangle
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


attributes : DragTarget -> Model -> List (Svg.Attribute Msg)
attributes dragTarget model =
    if Drag.isHovering dragTarget model.dragState || Drag.isDragging dragTarget model.dragState then
        [ Svg.Attributes.fill "lightblue", Svg.Attributes.stroke "black" ]
    else
        [ Svg.Attributes.fill "white", Svg.Attributes.stroke "black" ]


pointOptions : DragTarget -> Model -> Svg.PointOptions Msg
pointOptions dragTarget model =
    { radius = 5
    , attributes = attributes dragTarget model
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
