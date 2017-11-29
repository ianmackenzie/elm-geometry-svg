module Drag exposing (..)

import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Svg.Interaction as Interaction exposing (Interaction)
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes


type Target
    = Triangle
    | Vertex0
    | Vertex1
    | Vertex2
    | Edge0
    | Edge1
    | Edge2


type Msg
    = InteractionMsg (Interaction.Msg Target)


type alias Model =
    { interactionModel : Interaction.Model Target
    , triangle : Triangle2d
    }


init : ( Model, Cmd Msg )
init =
    ( { interactionModel = Interaction.init
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
        Interaction.container InteractionMsg boundingBox <|
            [ Svg.triangle2d (triangleAttributes model) model.triangle
            , Svg.lineSegment2d (edgeAttributes Edge0 model) edge0
            , Svg.lineSegment2d (edgeAttributes Edge1 model) edge1
            , Svg.lineSegment2d (edgeAttributes Edge2 model) edge2
            , Svg.point2d (pointOptions Vertex0 model) p0
            , Svg.point2d (pointOptions Vertex1 model) p1
            , Svg.point2d (pointOptions Vertex2 model) p2
            , case Interaction.selectionBox model.interactionModel of
                Just ( startPoint, endPoint, modifiers ) ->
                    Svg.boundingBox2d
                        [ Svg.Attributes.fill "rgba(0, 63, 255, 0.1)"
                        , Svg.Attributes.stroke "blue"
                        ]
                        (Point2d.hull startPoint endPoint)

                Nothing ->
                    Svg.text ""
            , Svg.g []
                [ Interaction.triangleHandle model.triangle
                    { target = Triangle
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Interaction.lineSegmentHandle edge0
                    { target = Edge0
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Interaction.lineSegmentHandle edge1
                    { target = Edge1
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Interaction.lineSegmentHandle edge2
                    { target = Edge2
                    , padding = 10
                    , renderBounds = boundingBox
                    }
                , Interaction.pointHandle p0
                    { target = Vertex0
                    , radius = 10
                    , renderBounds = boundingBox
                    }
                , Interaction.pointHandle p1
                    { target = Vertex1
                    , radius = 10
                    , renderBounds = boundingBox
                    }
                , Interaction.pointHandle p2
                    { target = Vertex2
                    , radius = 10
                    , renderBounds = boundingBox
                    }
                ]
                |> Svg.map InteractionMsg
            ]


isActive : Target -> Model -> Bool
isActive dragTarget model =
    Interaction.isHovering dragTarget model.interactionModel
        || Interaction.isDragging dragTarget model.interactionModel


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


edgeAttributes : Target -> Model -> List (Svg.Attribute Msg)
edgeAttributes dragTarget model =
    let
        color =
            if isActive dragTarget model then
                "rgb(63, 127, 255)"
            else
                "black"
    in
    [ Svg.Attributes.stroke color ]


pointOptions : Target -> Model -> Svg.PointOptions Msg
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


constrainBy : Interaction.Modifiers -> Vector2d -> Vector2d
constrainBy modifiers displacement =
    if modifiers.ctrl then
        case Vector2d.lengthAndDirection displacement of
            Just ( length, direction ) ->
                let
                    angle =
                        Direction2d.angle direction

                    snapIncrement =
                        degrees 45

                    snappedAngle =
                        toFloat (round (angle / snapIncrement)) * snapIncrement

                    snappedDirection =
                        Direction2d.fromAngle snappedAngle
                in
                Vector2d.projectionIn snappedDirection displacement

            Nothing ->
                Vector2d.zero
    else
        displacement


setInteractionModel : Interaction.Model Target -> Model -> Model
setInteractionModel updatedInteractionModel model =
    { model | interactionModel = updatedInteractionModel }


handleInteraction : Maybe (Interaction Target) -> Model -> Model
handleInteraction interaction model =
    case interaction of
        Nothing ->
            model

        Just (Interaction.Drag target startPoint endPoint modifiers) ->
            performDrag target startPoint endPoint modifiers model

        Just (Interaction.Click target modifiers) ->
            let
                _ =
                    Debug.log "Clicked" target
            in
            model

        Just (Interaction.Release target) ->
            let
                _ =
                    Debug.log "Released" target
            in
            model

        Just (Interaction.BoxSelect startPoint endPoint modifiers) ->
            let
                _ =
                    Debug.log "Box select" modifiers
            in
            model


performDrag : Target -> Point2d -> Point2d -> Interaction.Modifiers -> Model -> Model
performDrag target startPoint endPoint modifiers model =
    let
        displacement =
            Vector2d.from startPoint endPoint |> constrainBy modifiers

        ( p0, p1, p2 ) =
            Triangle2d.vertices model.triangle

        translatePoint =
            Point2d.translateBy displacement

        updatedTriangle =
            case target of
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


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        InteractionMsg interactionMsg ->
            let
                ( updatedInteractionModel, maybeInteraction ) =
                    Interaction.update interactionMsg model.interactionModel
            in
            model
                |> setInteractionModel updatedInteractionModel
                |> handleInteraction maybeInteraction
                |> noCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Interaction.subscriptions model.interactionModel
        |> Sub.map InteractionMsg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
