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


type TriangleTarget
    = Triangle
    | Vertex0
    | Vertex1
    | Vertex2
    | Edge0
    | Edge1
    | Edge2


type Target
    = Triangle1 TriangleTarget
    | Triangle2 TriangleTarget


type Msg
    = InteractionMsg (Interaction.Msg Target)


type alias Model =
    { interactionModel : Interaction.Model Target
    , triangle1 : Triangle2d
    , triangle2 : Triangle2d
    , selectedVertices : List Target
    }


init : ( Model, Cmd Msg )
init =
    ( { interactionModel = Interaction.initWith [ Interaction.dragThreshold 5 ]
      , triangle1 =
            Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 100, 200 )
                , Point2d.fromCoordinates ( 300, 200 )
                , Point2d.fromCoordinates ( 100, 400 )
                )
      , triangle2 =
            Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 500, 400 )
                , Point2d.fromCoordinates ( 300, 400 )
                , Point2d.fromCoordinates ( 500, 200 )
                )
      , selectedVertices = []
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
        drawTriangle tag triangle =
            let
                ( p0, p1, p2 ) =
                    Triangle2d.vertices triangle

                edge0 =
                    LineSegment2d.from p0 p1

                edge1 =
                    LineSegment2d.from p1 p2

                edge2 =
                    LineSegment2d.from p2 p0
            in
            Svg.g []
                [ Svg.triangle2d
                    (triangleAttributes (tag Triangle) model)
                    triangle
                , Svg.lineSegment2d (edgeAttributes (tag Edge0) model) edge0
                , Svg.lineSegment2d (edgeAttributes (tag Edge1) model) edge1
                , Svg.lineSegment2d (edgeAttributes (tag Edge2) model) edge2
                , Svg.point2d (pointOptions (tag Vertex0) model) p0
                , Svg.point2d (pointOptions (tag Vertex1) model) p1
                , Svg.point2d (pointOptions (tag Vertex2) model) p2
                , Svg.g []
                    [ Interaction.triangleHandle triangle
                        { target = tag Triangle
                        , padding = 10
                        , renderBounds = boundingBox
                        }
                    , Interaction.lineSegmentHandle edge0
                        { target = tag Edge0
                        , padding = 10
                        , renderBounds = boundingBox
                        }
                    , Interaction.lineSegmentHandle edge1
                        { target = tag Edge1
                        , padding = 10
                        , renderBounds = boundingBox
                        }
                    , Interaction.lineSegmentHandle edge2
                        { target = tag Edge2
                        , padding = 10
                        , renderBounds = boundingBox
                        }
                    , Interaction.pointHandle p0
                        { target = tag Vertex0
                        , radius = 10
                        , renderBounds = boundingBox
                        }
                    , Interaction.pointHandle p1
                        { target = tag Vertex1
                        , radius = 10
                        , renderBounds = boundingBox
                        }
                    , Interaction.pointHandle p2
                        { target = tag Vertex2
                        , radius = 10
                        , renderBounds = boundingBox
                        }
                    ]
                    |> Svg.map InteractionMsg
                ]
    in
    Svg.render2d boundingBox <|
        Interaction.container InteractionMsg boundingBox <|
            [ drawTriangle Triangle1 model.triangle1
            , drawTriangle Triangle2 model.triangle2
            , case Interaction.selectionBox model.interactionModel of
                Just ( startPoint, endPoint, modifiers ) ->
                    Svg.boundingBox2d
                        [ Svg.Attributes.fill "rgba(0, 63, 255, 0.1)"
                        , Svg.Attributes.stroke "blue"
                        ]
                        (Point2d.hull startPoint endPoint)

                Nothing ->
                    Svg.text ""
            ]


isActive : Target -> Model -> Bool
isActive dragTarget model =
    Interaction.isHovering dragTarget model.interactionModel
        || Interaction.isDragging dragTarget model.interactionModel
        || List.member dragTarget model.selectedVertices


triangleAttributes : Target -> Model -> List (Svg.Attribute Msg)
triangleAttributes dragTarget model =
    let
        fillColor =
            if isActive dragTarget model then
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


setSelectedVertices : List Target -> Model -> Model
setSelectedVertices updatedSelectedVertices model =
    { model | selectedVertices = updatedSelectedVertices }


isVertex : Target -> Bool
isVertex target =
    let
        isTriangleVertex t =
            t == Vertex0 || t == Vertex1 || t == Vertex2
    in
    case target of
        Triangle1 target ->
            isTriangleVertex target

        Triangle2 target ->
            isTriangleVertex target


verticesIn : BoundingBox2d -> (TriangleTarget -> Target) -> Triangle2d -> List Target
verticesIn boundingBox tag triangle =
    let
        ( p0, p1, p2 ) =
            Triangle2d.vertices triangle

        vertexTarget triangleTarget point =
            if BoundingBox2d.contains point boundingBox then
                Just triangleTarget
            else
                Nothing
    in
    List.filterMap identity
        [ vertexTarget (tag Vertex0) p0
        , vertexTarget (tag Vertex1) p1
        , vertexTarget (tag Vertex2) p2
        ]


handleInteraction : Maybe (Interaction Target) -> Model -> Model
handleInteraction interaction model =
    case interaction of
        Nothing ->
            model

        Just (Interaction.Drag Nothing { startPoint, currentPoint } modifiers) ->
            let
                boundingBox =
                    Point2d.hull startPoint currentPoint

                targets =
                    verticesIn boundingBox Triangle1 model.triangle1
                        ++ verticesIn boundingBox Triangle2 model.triangle2
            in
            setSelectedVertices targets model

        Just (Interaction.Drag (Just target) { previousPoint, currentPoint } modifiers) ->
            if List.member target model.selectedVertices then
                let
                    applyDrag selectedVertex model =
                        performDrag selectedVertex
                            previousPoint
                            currentPoint
                            modifiers
                            model
                in
                List.foldl applyDrag model model.selectedVertices
            else
                performDrag target previousPoint currentPoint modifiers model
                    |> setSelectedVertices []

        Just (Interaction.Click Nothing modifiers) ->
            if modifiers.ctrl || modifiers.shift then
                model
            else
                { model | selectedVertices = [] }

        Just (Interaction.Click (Just target) modifiers) ->
            let
                newSelectedVertices =
                    if isVertex target then
                        if modifiers.shift then
                            if List.member target model.selectedVertices then
                                model.selectedVertices
                            else
                                target :: model.selectedVertices
                        else if modifiers.ctrl then
                            if List.member target model.selectedVertices then
                                List.filter ((/=) target) model.selectedVertices
                            else
                                target :: model.selectedVertices
                        else
                            [ target ]
                    else if modifiers.ctrl || modifiers.shift then
                        model.selectedVertices
                    else
                        []
            in
            { model | selectedVertices = newSelectedVertices }

        Just (Interaction.Release target _ modifiers) ->
            model


performDrag : Target -> Point2d -> Point2d -> Interaction.Modifiers -> Model -> Model
performDrag target startPoint endPoint modifiers model =
    let
        displacement =
            Vector2d.from startPoint endPoint |> constrainBy modifiers

        translatePoint =
            Point2d.translateBy displacement

        updateTriangle triangleTarget triangle =
            let
                ( p0, p1, p2 ) =
                    Triangle2d.vertices triangle
            in
            case triangleTarget of
                Triangle ->
                    Triangle2d.translateBy displacement triangle

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
    case target of
        Triangle1 triangleTarget ->
            { model
                | triangle1 = updateTriangle triangleTarget model.triangle1
            }

        Triangle2 triangleTarget ->
            { model
                | triangle2 = updateTriangle triangleTarget model.triangle2
            }


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
