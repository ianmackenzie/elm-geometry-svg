module OpenSolid.Svg.Drag
    exposing
        ( Event
        , Model
        , State
        , apply
        , customHandle
          --, direction
        , directionTipHandle
        , isDragging
        , isHovering
        , lineSegment
        , lineSegmentHandle
        , none
        , point
        , pointHandle
        , process
          --, vector
        , subscriptions
        , triangle
        , triangleHandle
        , vectorTipHandle
        )

import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Mouse
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Svg.Internal as Internal
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


type State t
    = Resting
    | Hovering t
    | Dragging
        { target : t
        , hoverTarget : Maybe t
        , lastPoint : Point2d
        , x0 : Float
        , y0 : Float
        }


type Event t
    = Entered t
    | Left t
    | StartedDrag { target : t, startPoint : Point2d, x0 : Float, y0 : Float }
    | DraggedTo Point2d
    | EndedDrag


none : State t
none =
    Resting


process : Event t -> State t -> ( State t, Maybe { target : t, startPoint : Point2d, endPoint : Point2d } )
process event state =
    let
        unexpected () =
            let
                _ =
                    Debug.log "Unexpected state/event combination"
                        ( state, event )

                _ =
                    Debug.log "Please raise an issue at"
                        "https://github.com/opensolid/svg/issues/new"
            in
            ( Resting, Nothing )
    in
    case ( state, event ) of
        ( Resting, Entered target ) ->
            ( Hovering target, Nothing )

        ( Hovering hoverTarget, StartedDrag { target, startPoint, x0, y0 } ) ->
            if hoverTarget == target then
                ( Dragging
                    { target = target
                    , hoverTarget = Just target
                    , lastPoint = startPoint
                    , x0 = x0
                    , y0 = y0
                    }
                , Nothing
                )
            else
                unexpected ()

        ( Hovering hoverTarget, Left previousTarget ) ->
            if previousTarget == hoverTarget then
                ( Resting, Nothing )
            else
                unexpected ()

        ( Dragging properties, Entered hoverTarget ) ->
            if properties.hoverTarget == Nothing then
                ( Dragging { properties | hoverTarget = Just hoverTarget }, Nothing )
            else
                unexpected ()

        ( Dragging properties, Left hoverTarget ) ->
            if properties.hoverTarget == Just hoverTarget then
                ( Dragging { properties | hoverTarget = Nothing }, Nothing )
            else
                unexpected ()

        ( Dragging properties, EndedDrag ) ->
            case properties.hoverTarget of
                Nothing ->
                    ( Resting, Nothing )

                Just hoverTarget ->
                    ( Hovering hoverTarget, Nothing )

        ( Dragging { target, hoverTarget, lastPoint, x0, y0 }, DraggedTo endPoint ) ->
            ( Dragging
                { target = target
                , hoverTarget = hoverTarget
                , lastPoint = endPoint
                , x0 = x0
                , y0 = y0
                }
            , Just
                { target = target
                , startPoint = lastPoint
                , endPoint = endPoint
                }
            )

        _ ->
            unexpected ()


type alias Model m t =
    { m | dragState : State t }


apply : (t -> Point2d -> Point2d -> Model m t -> Model m t) -> Event t -> Model m t -> ( Model m t, Cmd msg )
apply performDrag event model =
    let
        ( updatedState, drag ) =
            process event model.dragState

        updatedModel =
            { model | dragState = updatedState }
    in
    case drag of
        Nothing ->
            ( updatedModel, Cmd.none )

        Just { target, startPoint, endPoint } ->
            ( performDrag target startPoint endPoint updatedModel, Cmd.none )


subscriptions : State t -> Sub (Event t)
subscriptions state =
    case state of
        Dragging { x0, y0 } ->
            let
                toEvent { x, y } =
                    DraggedTo <|
                        Point2d.fromCoordinates
                            ( x0 + toFloat x
                            , y0 - toFloat y
                            )
            in
            Sub.batch
                [ Mouse.moves toEvent
                , Mouse.ups (always EndedDrag)
                ]

        _ ->
            Sub.none


type alias Coordinates =
    { clientX : Float
    , clientY : Float
    , pageX : Float
    , pageY : Float
    }


decodeCoordinates : Decoder Coordinates
decodeCoordinates =
    Decode.map4 Coordinates
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


customHandle : BoundingBox2d -> t -> Svg Never -> Svg (Event t)
customHandle boundingBox target shape =
    let
        attributes =
            [ Html.Events.onWithOptions
                "mousedown"
                { stopPropagation = True
                , preventDefault = True
                }
                (decodeCoordinates
                    |> Decode.map
                        (\{ clientX, clientY, pageX, pageY } ->
                            let
                                x =
                                    BoundingBox2d.minX boundingBox + clientX

                                y =
                                    BoundingBox2d.maxY boundingBox - clientY

                                startPoint =
                                    Point2d.fromCoordinates ( x, y )
                            in
                            StartedDrag
                                { target = target
                                , startPoint = startPoint
                                , x0 = x - pageX
                                , y0 = y + pageY
                                }
                        )
                )
            , Svg.Events.onMouseOver (Entered target)
            , Svg.Events.onMouseOut (Left target)
            ]
    in
    Svg.g attributes [ shape |> Svg.map never ]


transparentFill : Svg.Attribute msg
transparentFill =
    Svg.Attributes.fill "transparent"


noStroke : Svg.Attribute msg
noStroke =
    Svg.Attributes.stroke "none"


pointHandle : BoundingBox2d -> t -> Point2d -> Float -> Svg (Event t)
pointHandle boundingBox target point radius =
    customHandle boundingBox target <|
        Svg.point2d
            { radius = radius
            , attributes = [ noStroke, transparentFill ]
            }
            point


thickened : Float -> List (Svg.Attribute msg)
thickened padding =
    [ Svg.Attributes.fill "transparent"
    , Svg.Attributes.stroke "transparent"
    , Svg.Attributes.strokeWidth (toString (2 * padding))
    , Svg.Attributes.strokeLinejoin "round"
    , Svg.Attributes.strokeLinecap "round"
    ]


lineSegmentHandle : BoundingBox2d -> t -> LineSegment2d -> Float -> Svg (Event t)
lineSegmentHandle boundingBox target lineSegment padding =
    customHandle boundingBox target <|
        Svg.lineSegment2d (thickened padding) lineSegment


triangleHandle : BoundingBox2d -> t -> Triangle2d -> Float -> Svg (Event t)
triangleHandle boundingBox target triangle padding =
    customHandle boundingBox target <|
        Svg.triangle2d (thickened padding) triangle


vectorTipHandle : BoundingBox2d -> t -> Point2d -> Vector2d -> { a | tipWidth : Float, tipLength : Float } -> Float -> Svg (Event t)
vectorTipHandle boundingBox target basePoint vector vectorOptions padding =
    case Vector2d.lengthAndDirection vector of
        Just ( length, direction ) ->
            let
                tip =
                    Internal.tip vectorOptions basePoint length direction
            in
            customHandle boundingBox target <|
                Svg.triangle2d (thickened padding) tip

        Nothing ->
            pointHandle boundingBox target basePoint padding


directionTipHandle : BoundingBox2d -> t -> Point2d -> Direction2d -> { a | length : Float, tipWidth : Float, tipLength : Float } -> Float -> Svg (Event t)
directionTipHandle boundingBox target basePoint direction directionOptions padding =
    let
        vector =
            Vector2d.with
                { length = directionOptions.length
                , direction = direction
                }
    in
    vectorTipHandle boundingBox target basePoint vector directionOptions padding


isHovering : t -> State t -> Bool
isHovering target state =
    case state of
        Resting ->
            False

        Hovering hoverTarget ->
            target == hoverTarget

        Dragging _ ->
            False


isDragging : t -> State t -> Bool
isDragging target state =
    case state of
        Resting ->
            False

        Hovering _ ->
            False

        Dragging properties ->
            target == properties.target


point : Point2d -> Point2d -> Point2d -> Point2d
point dragStart dragEnd =
    Point2d.translateBy (Vector2d.from dragStart dragEnd)


lineSegment : Point2d -> Point2d -> LineSegment2d -> LineSegment2d
lineSegment dragStart dragEnd =
    LineSegment2d.translateBy (Vector2d.from dragStart dragEnd)


triangle : Point2d -> Point2d -> Triangle2d -> Triangle2d
triangle dragStart dragEnd =
    Triangle2d.translateBy (Vector2d.from dragStart dragEnd)



--vector : Point2d -> Point2d -> Vector2d -> Vector2d
--vector dragStart dragEnd =
--    Vector2d.sum (Vector2d.from dragStart dragEnd)
--direction : Point2d -> Point2d -> Point2d -> Direction2d -> Direction2d
--direction dragStart dragEnd basePoint =
--    let
--        startDirection =
--            Direction2d.from basePoint dragStart
--        endDirection =
--            Direction2d.from basePoint dragEnd
--        angle =
--            Maybe.map2 Direction2d.angleFrom
--                startDirection
--                endDirection
--                |> Maybe.withDefault 0
--    in
--    Direction2d.rotateBy angle
