module Drag exposing (..)

import Html exposing (Html)
import Html.Attributes
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
import Time exposing (Time)


type TriangleComponent
    = Interior
    | Vertex0
    | Vertex1
    | Vertex2
    | Edge0
    | Edge1
    | Edge2


type Target
    = TriangleTarget { triangleIndex : Int, component : TriangleComponent }
    | Elsewhere


type Msg
    = InteractionMsg (Interaction.Msg Target)


type alias Model =
    { interactionModel : Interaction.Model Target
    , triangles : List Triangle2d
    , selectedVertices : List Target
    }


init : ( Model, Cmd Msg )
init =
    ( { interactionModel =
            Interaction.modelWith
                [ Interaction.dragThresholdDistance 5
                , Interaction.longPressThresholdTime (0.5 * Time.second)
                ]
      , triangles =
            [ Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 200, 150 )
                , Point2d.fromCoordinates ( 300, 200 )
                , Point2d.fromCoordinates ( 100, 400 )
                )
            , Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 100, 200 )
                , Point2d.fromCoordinates ( 500, 300 )
                , Point2d.fromCoordinates ( 300, 350 )
                )
            , Triangle2d.fromVertices
                ( Point2d.fromCoordinates ( 500, 400 )
                , Point2d.fromCoordinates ( 350, 400 )
                , Point2d.fromCoordinates ( 400, 200 )
                )
            ]
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
        drawTriangle index triangle =
            let
                ( p0, p1, p2 ) =
                    Triangle2d.vertices triangle

                edge0 =
                    LineSegment2d.from p0 p1

                edge1 =
                    LineSegment2d.from p1 p2

                edge2 =
                    LineSegment2d.from p2 p0

                target component =
                    TriangleTarget
                        { triangleIndex = index
                        , component = component
                        }
            in
            Svg.g []
                [ Svg.triangle2d
                    (triangleAttributes (target Interior) model)
                    triangle
                , Svg.lineSegment2d (edgeAttributes (target Edge0) model) edge0
                , Svg.lineSegment2d (edgeAttributes (target Edge1) model) edge1
                , Svg.lineSegment2d (edgeAttributes (target Edge2) model) edge2
                , Svg.point2d (pointAttributes (target Vertex0) model) p0
                , Svg.point2d (pointAttributes (target Vertex1) model) p1
                , Svg.point2d (pointAttributes (target Vertex2) model) p2
                , Svg.g []
                    [ Interaction.triangleHandle triangle
                        { target = target Interior
                        , padding = 10
                        }
                    , Interaction.lineSegmentHandle edge0
                        { target = target Edge0
                        , padding = 10
                        }
                    , Interaction.lineSegmentHandle edge1
                        { target = target Edge1
                        , padding = 10
                        }
                    , Interaction.lineSegmentHandle edge2
                        { target = target Edge2
                        , padding = 10
                        }
                    , Interaction.pointHandle p0
                        { target = target Vertex0
                        , radius = 10
                        }
                    , Interaction.pointHandle p1
                        { target = target Vertex1
                        , radius = 10
                        }
                    , Interaction.pointHandle p2
                        { target = target Vertex2
                        , radius = 10
                        }
                    ]
                    |> Svg.map InteractionMsg
                ]
    in
    Html.div
        [ Html.Attributes.style
            [ ( "border", "1px solid black" )
            , ( "width", "600px" )
            , ( "height", "600px" )
            ]
        ]
        [ Svg.render2d boundingBox <|
            Interaction.container InteractionMsg
                { target = Elsewhere
                , renderBounds = boundingBox
                }
                [ Svg.g [] (List.indexedMap drawTriangle model.triangles)
                , case Interaction.dragState model.interactionModel of
                    Nothing ->
                        Svg.text ""

                    Just { target, startPoint, currentPoint, modifiers } ->
                        if target == Elsewhere then
                            Svg.boundingBox2d
                                [ Svg.Attributes.fill "rgba(0, 63, 255, 0.1)"
                                , Svg.Attributes.stroke "blue"
                                , Svg.Attributes.pointerEvents "none"
                                ]
                                (Point2d.hull startPoint currentPoint)
                        else
                            Svg.text ""
                ]
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


pointAttributes : Target -> Model -> List (Svg.Attribute Msg)
pointAttributes dragTarget model =
    let
        fillColor =
            if isActive dragTarget model then
                "rgb(63, 127, 255)"
            else
                "white"
    in
    [ Svg.Attributes.fill fillColor, Svg.Attributes.stroke "black" ]


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


isVertex : TriangleComponent -> Bool
isVertex component =
    component == Vertex0 || component == Vertex1 || component == Vertex2


verticesIn : BoundingBox2d -> Int -> Triangle2d -> List Target
verticesIn boundingBox index triangle =
    let
        ( p0, p1, p2 ) =
            Triangle2d.vertices triangle

        vertexTarget component point =
            if BoundingBox2d.contains point boundingBox then
                Just <|
                    TriangleTarget
                        { triangleIndex = index
                        , component = component
                        }
            else
                Nothing
    in
    List.filterMap identity
        [ vertexTarget Vertex0 p0
        , vertexTarget Vertex1 p1
        , vertexTarget Vertex2 p2
        ]


handleInteraction : Interaction Target -> Model -> Model
handleInteraction interaction model =
    case interaction of
        Interaction.Drag ((TriangleTarget _) as target) modifiers { previousPoint, currentPoint } ->
            if List.member target model.selectedVertices then
                let
                    applyDrag selectedVertex model =
                        performDrag
                            selectedVertex
                            previousPoint
                            currentPoint
                            modifiers
                            model
                in
                List.foldl applyDrag model model.selectedVertices
            else
                performDrag
                    target
                    previousPoint
                    currentPoint
                    modifiers
                    model
                    |> setSelectedVertices []

        Interaction.Drag Elsewhere modifiers { startPoint, currentPoint } ->
            let
                boundingBox =
                    Point2d.hull startPoint currentPoint

                targets =
                    List.indexedMap (verticesIn boundingBox) model.triangles
                        |> List.concat
            in
            setSelectedVertices targets model

        Interaction.Click Elsewhere modifiers ->
            if modifiers.ctrl || modifiers.shift then
                model
            else
                { model | selectedVertices = [] }

        Interaction.Click ((TriangleTarget { triangleIndex, component }) as target) modifiers ->
            let
                newSelectedVertices =
                    if isVertex component then
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

        Interaction.Release target _ modifiers ->
            model

        Interaction.Tap target ->
            let
                _ =
                    Debug.log "Tapped" target
            in
            model

        Interaction.LongPress target ->
            let
                _ =
                    Debug.log "Pressed" target
            in
            model

        Interaction.Slide target _ ->
            let
                _ =
                    Debug.log "Slide" target
            in
            model

        Interaction.Scroll target modifiers amount ->
            let
                _ =
                    Debug.log "Scroll" amount
            in
            model

        Interaction.Lift target ->
            let
                _ =
                    Debug.log "Lift" target
            in
            model


performDrag : Target -> Point2d -> Point2d -> Interaction.Modifiers -> Model -> Model
performDrag target startPoint endPoint modifiers model =
    case target of
        TriangleTarget { triangleIndex, component } ->
            let
                displacement =
                    Vector2d.from startPoint endPoint |> constrainBy modifiers

                translatePoint =
                    Point2d.translateBy displacement

                updateTriangle index triangle =
                    if index == triangleIndex then
                        let
                            ( p0, p1, p2 ) =
                                Triangle2d.vertices triangle
                        in
                        case component of
                            Interior ->
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
                    else
                        triangle

                updatedTriangles =
                    List.indexedMap updateTriangle model.triangles
            in
            { model | triangles = updatedTriangles }

        Elsewhere ->
            model


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        InteractionMsg interactionMsg ->
            let
                ( updatedInteractionModel, interactions ) =
                    Interaction.update interactionMsg model.interactionModel
            in
            model
                |> setInteractionModel updatedInteractionModel
                |> (\model -> List.foldl handleInteraction model interactions)
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
