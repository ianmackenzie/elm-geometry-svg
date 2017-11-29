module OpenSolid.Svg.Interaction
    exposing
        ( Interaction(..)
        , Model
        , Modifiers
        , Msg
        , container
        , customHandle
        , directionTipHandle
        , init
        , isDragging
        , isHovering
        , lineSegmentHandle
        , pointHandle
        , rotationAround
        , selectionBox
        , subscriptions
        , triangleHandle
        , update
        , vectorTipHandle
        )

import AnimationFrame
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Keyboard
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


type alias Modifiers =
    { ctrl : Bool
    , shift : Bool
    }


type State t
    = Resting
    | Hovering t
    | Dragging
        { target : Maybe t
        , hoverTarget : Maybe t
        , startPoint : Point2d
        , dragStarted : Bool
        , lastPoint : Point2d
        , pageOrigin : Point2d
        , modifiers : Modifiers
        }


type Model t
    = Model
        { config : { dragThreshold : Float }
        , state : State t
        , justFinishedDrag : Bool
        }


type Msg t
    = Entered t
    | Left t
    | MouseDown
        { target : Maybe t
        , point : Point2d
        , pageOrigin : Point2d
        , modifiers : Modifiers
        }
    | DraggedTo Point2d
    | MouseUp
    | KeyDown Int
    | KeyUp Int
    | Tick


type Interaction t
    = Click (Maybe t) Modifiers
    | Drag t Point2d Point2d Modifiers
    | Release t
    | BoxSelect Point2d Point2d Modifiers


init : Model t
init =
    Model
        { config = { dragThreshold = 0 }
        , state = Resting
        , justFinishedDrag = False
        }


rotationAround : Point2d -> Point2d -> Point2d -> Float
rotationAround point start end =
    let
        startDirection =
            Direction2d.from point start

        endDirection =
            Direction2d.from point end
    in
    Maybe.map2 Direction2d.angleFrom
        startDirection
        endDirection
        |> Maybe.withDefault 0


keyDown : Int -> Modifiers -> Modifiers
keyDown code modifiers =
    case code of
        16 ->
            { modifiers | shift = True }

        17 ->
            { modifiers | ctrl = True }

        _ ->
            modifiers


keyUp : Int -> Modifiers -> Modifiers
keyUp code modifiers =
    case code of
        16 ->
            { modifiers | shift = False }

        17 ->
            { modifiers | ctrl = False }

        _ ->
            modifiers


update : Msg t -> Model t -> ( Model t, Maybe (Interaction t) )
update message ((Model { config, state, justFinishedDrag }) as model) =
    let
        unexpected () =
            let
                _ =
                    Debug.log "Unexpected state/message combination"
                        ( state, message )

                _ =
                    Debug.log "Please raise an issue at"
                        "https://github.com/opensolid/svg/issues/new"
            in
            ( model, Nothing )

        setState updatedState =
            Model
                { config = config
                , state = updatedState
                , justFinishedDrag = justFinishedDrag
                }

        finishDrag updatedState =
            Model
                { config = config
                , state = updatedState
                , justFinishedDrag = True
                }
    in
    case ( state, message ) of
        ( Resting, Entered target ) ->
            ( setState (Hovering target), Nothing )

        ( Resting, MouseDown { target, point, pageOrigin, modifiers } ) ->
            case target of
                Nothing ->
                    ( setState <|
                        Dragging
                            { target = Nothing
                            , hoverTarget = Nothing
                            , startPoint = point
                            , lastPoint = point
                            , dragStarted = False
                            , pageOrigin = pageOrigin
                            , modifiers = modifiers
                            }
                    , Nothing
                    )

                Just dragTarget ->
                    unexpected ()

        ( Resting, DraggedTo _ ) ->
            if justFinishedDrag then
                ( Model
                    { config = config
                    , state = state
                    , justFinishedDrag = False
                    }
                , Nothing
                )
            else
                unexpected ()

        ( Hovering hoverTarget, MouseDown { target, point, pageOrigin, modifiers } ) ->
            if target == Just hoverTarget then
                ( setState <|
                    Dragging
                        { target = target
                        , hoverTarget = target
                        , startPoint = point
                        , lastPoint = point
                        , dragStarted = False
                        , pageOrigin = pageOrigin
                        , modifiers = modifiers
                        }
                , Nothing
                )
            else
                unexpected ()

        ( Hovering hoverTarget, Left previousTarget ) ->
            if previousTarget == hoverTarget then
                ( setState Resting, Nothing )
            else
                unexpected ()

        ( Hovering _, DraggedTo _ ) ->
            if justFinishedDrag then
                ( Model
                    { config = config
                    , state = state
                    , justFinishedDrag = False
                    }
                , Nothing
                )
            else
                unexpected ()

        ( Dragging properties, Entered hoverTarget ) ->
            if properties.hoverTarget == Nothing then
                ( setState <|
                    Dragging { properties | hoverTarget = Just hoverTarget }
                , Nothing
                )
            else
                unexpected ()

        ( Dragging properties, Left hoverTarget ) ->
            if properties.hoverTarget == Just hoverTarget then
                ( setState (Dragging { properties | hoverTarget = Nothing })
                , Nothing
                )
            else
                unexpected ()

        ( Dragging properties, KeyDown code ) ->
            let
                updatedModifiers =
                    keyDown code properties.modifiers
            in
            ( setState (Dragging { properties | modifiers = updatedModifiers })
            , Nothing
            )

        ( Dragging properties, KeyUp code ) ->
            let
                updatedModifiers =
                    keyUp code properties.modifiers
            in
            ( setState (Dragging { properties | modifiers = updatedModifiers })
            , Nothing
            )

        ( Dragging properties, MouseUp ) ->
            let
                { target, hoverTarget, startPoint, lastPoint, modifiers, dragStarted } =
                    properties

                interaction =
                    if dragStarted then
                        case target of
                            Just dragTarget ->
                                Release dragTarget

                            Nothing ->
                                BoxSelect startPoint lastPoint modifiers
                    else
                        Click target modifiers
            in
            case hoverTarget of
                Nothing ->
                    ( finishDrag Resting, Just interaction )

                Just hoverTarget ->
                    ( finishDrag (Hovering hoverTarget), Just interaction )

        ( Dragging properties, DraggedTo point ) ->
            let
                { target, modifiers, startPoint, lastPoint } =
                    properties

                dragDistance =
                    Point2d.distanceFrom startPoint point
            in
            if dragDistance > config.dragThreshold then
                case target of
                    Just dragTarget ->
                        ( setState <|
                            Dragging
                                { properties | lastPoint = point, dragStarted = True }
                        , Just (Drag dragTarget lastPoint point modifiers)
                        )

                    Nothing ->
                        ( setState <|
                            Dragging
                                { properties | lastPoint = point, dragStarted = True }
                        , Just (BoxSelect lastPoint point modifiers)
                        )
            else
                ( model, Nothing )

        ( _, Tick ) ->
            ( Model
                { config = config
                , state = state
                , justFinishedDrag = False
                }
            , Nothing
            )

        _ ->
            unexpected ()


subscriptions : Model t -> Sub (Msg t)
subscriptions (Model { state, justFinishedDrag }) =
    case state of
        Dragging { pageOrigin } ->
            let
                ( x0, y0 ) =
                    Point2d.coordinates pageOrigin

                toDragEvent { x, y } =
                    DraggedTo <|
                        Point2d.fromCoordinates
                            ( x0 + toFloat x
                            , y0 - toFloat y
                            )
            in
            Sub.batch
                [ Mouse.moves toDragEvent
                , Mouse.ups (always MouseUp)
                , Keyboard.ups KeyUp
                , Keyboard.downs KeyDown
                ]

        _ ->
            if justFinishedDrag then
                AnimationFrame.times (always Tick)
            else
                Sub.none


container : (Msg t -> msg) -> BoundingBox2d -> List (Svg msg) -> Svg msg
container tagger renderBounds children =
    let
        onMouseDown =
            Html.Events.onWithOptions "mousedown"
                { stopPropagation = True
                , preventDefault = True
                }
                (decodeMouseDown Nothing renderBounds |> Decode.map tagger)

        background =
            Svg.boundingBox2d [ transparentFill, noStroke ] renderBounds
    in
    Svg.g [ onMouseDown ] (background :: children)


type alias Start =
    { clientX : Float
    , clientY : Float
    , pageX : Float
    , pageY : Float
    , modifiers : Modifiers
    }


decodeModifiers : Decoder Modifiers
decodeModifiers =
    Decode.map2 Modifiers
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)


decodeStart : Decoder Start
decodeStart =
    Decode.map5 Start
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
        decodeModifiers


decodeMouseDown : Maybe t -> BoundingBox2d -> Decoder (Msg t)
decodeMouseDown target renderBounds =
    decodeStart
        |> Decode.map
            (\{ clientX, clientY, pageX, pageY, modifiers } ->
                let
                    x =
                        BoundingBox2d.minX renderBounds + clientX

                    y =
                        BoundingBox2d.maxY renderBounds - clientY

                    point =
                        Point2d.fromCoordinates ( x, y )

                    x0 =
                        x - pageX

                    y0 =
                        y + pageY

                    pageOrigin =
                        Point2d.fromCoordinates ( x0, y0 )
                in
                MouseDown
                    { target = target
                    , point = point
                    , pageOrigin = pageOrigin
                    , modifiers = modifiers
                    }
            )


customHandle : Svg Never -> { target : t, renderBounds : BoundingBox2d } -> Svg (Msg t)
customHandle shape { target, renderBounds } =
    let
        attributes =
            [ Html.Events.onWithOptions "mousedown"
                { stopPropagation = True
                , preventDefault = True
                }
                (decodeMouseDown (Just target) renderBounds)
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


pointHandle : Point2d -> { target : t, radius : Float, renderBounds : BoundingBox2d } -> Svg (Msg t)
pointHandle point { target, radius, renderBounds } =
    let
        shape =
            Svg.point2d
                { radius = radius
                , attributes = [ noStroke, transparentFill ]
                }
                point
    in
    customHandle shape { target = target, renderBounds = renderBounds }


thickened : Float -> List (Svg.Attribute msg)
thickened padding =
    [ Svg.Attributes.fill "transparent"
    , Svg.Attributes.stroke "transparent"
    , Svg.Attributes.strokeWidth (toString (2 * padding))
    , Svg.Attributes.strokeLinejoin "round"
    , Svg.Attributes.strokeLinecap "round"
    ]


lineSegmentHandle : LineSegment2d -> { target : t, padding : Float, renderBounds : BoundingBox2d } -> Svg (Msg t)
lineSegmentHandle lineSegment { target, padding, renderBounds } =
    let
        shape =
            Svg.lineSegment2d (thickened padding) lineSegment
    in
    customHandle shape { target = target, renderBounds = renderBounds }


triangleHandle : Triangle2d -> { target : t, padding : Float, renderBounds : BoundingBox2d } -> Svg (Msg t)
triangleHandle triangle { target, padding, renderBounds } =
    let
        shape =
            Svg.triangle2d (thickened padding) triangle
    in
    customHandle shape { target = target, renderBounds = renderBounds }


vectorTipHandle : Point2d -> Vector2d -> { target : t, tipLength : Float, tipWidth : Float, padding : Float, renderBounds : BoundingBox2d } -> Svg (Msg t)
vectorTipHandle basePoint vector { target, tipLength, tipWidth, padding, renderBounds } =
    case Vector2d.lengthAndDirection vector of
        Just ( length, direction ) ->
            let
                shape =
                    Svg.triangle2d (thickened padding) <|
                        Internal.tip
                            { tipLength = tipLength, tipWidth = tipWidth }
                            basePoint
                            length
                            direction
            in
            customHandle shape { target = target, renderBounds = renderBounds }

        Nothing ->
            pointHandle basePoint
                { target = target
                , radius = padding
                , renderBounds = renderBounds
                }


directionTipHandle : Point2d -> Direction2d -> { length : Float, tipLength : Float, tipWidth : Float, target : t, padding : Float, renderBounds : BoundingBox2d } -> Svg (Msg t)
directionTipHandle basePoint direction { length, tipLength, tipWidth, padding, target, renderBounds } =
    let
        vector =
            Vector2d.with { length = length, direction = direction }
    in
    vectorTipHandle basePoint vector <|
        { target = target
        , tipLength = tipLength
        , tipWidth = tipWidth
        , padding = padding
        , renderBounds = renderBounds
        }


isHovering : t -> Model t -> Bool
isHovering target (Model { state }) =
    case state of
        Resting ->
            False

        Hovering hoverTarget ->
            target == hoverTarget

        Dragging _ ->
            False


isDragging : t -> Model t -> Bool
isDragging target (Model { state }) =
    case state of
        Resting ->
            False

        Hovering _ ->
            False

        Dragging properties ->
            properties.target == Just target


selectionBox : Model t -> Maybe ( Point2d, Point2d, Modifiers )
selectionBox (Model { state }) =
    case state of
        Resting ->
            Nothing

        Hovering _ ->
            Nothing

        Dragging { target, dragStarted, startPoint, lastPoint, modifiers } ->
            case target of
                Just dragTarget ->
                    Nothing

                Nothing ->
                    if dragStarted then
                        Just ( startPoint, lastPoint, modifiers )
                    else
                        Nothing
