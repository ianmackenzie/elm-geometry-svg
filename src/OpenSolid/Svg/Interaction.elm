module OpenSolid.Svg.Interaction
    exposing
        ( Interaction(..)
        , Model
        , Modifiers
        , Msg
        , Option
        , container
        , customHandle
        , directionTipHandle
        , dragTarget
        , dragThresholdDistance
        , hoverTarget
        , isDragging
        , isHovering
        , lineSegmentHandle
        , longPressThresholdTime
        , model
        , modelWith
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
import Time exposing (Time)


type alias Modifiers =
    { ctrl : Bool
    , shift : Bool
    }


type MouseState t
    = Resting
    | Hovering t
    | Dragging
        { target : Maybe t
        , hoverTarget : Maybe t
        , startPoint : Point2d
        , dragStarted : Bool
        , currentPoint : Point2d
        , pageOrigin : Point2d
        , modifiers : Modifiers
        }


type Model t
    = Model
        { config :
            { dragThresholdDistance : Float
            , longPressThresholdTime : Time
            }
        , mouseState : MouseState t
        , justFinishedDrag : Bool
        }


type Option
    = DragThresholdDistance Float
    | LongPressThresholdTime Time


type alias DragState t =
    { target : Maybe t
    , startPoint : Point2d
    , currentPoint : Point2d
    , modifiers : Modifiers
    }


type Msg t
    = Entered t
    | Left t
    | PrimaryMouseDown
        { target : Maybe t
        , point : Point2d
        , pageOrigin : Point2d
        , modifiers : Modifiers
        }
    | OtherMouseDown Point2d
    | DraggedTo Point2d
    | MouseUp Point2d
    | KeyDown Int
    | KeyUp Int
    | Tick


type Touch t
    = Touch Int (Maybe t) { startPoint : Point2d, previousPoint : Point2d, currentPoint : Point2d }


type Interaction t
    = Click (Maybe t) Modifiers
    | Drag (Maybe t) Modifiers { startPoint : Point2d, previousPoint : Point2d, currentPoint : Point2d }
    | Release (Maybe t) Modifiers { startPoint : Point2d, endPoint : Point2d }
    | Tap (List (Maybe t))
    | Press (List (Maybe t))
    | Gesture (List (Touch t))


model : Model t
model =
    modelWith []


dragThresholdDistance : Float -> Option
dragThresholdDistance =
    DragThresholdDistance


longPressThresholdTime : Time -> Option
longPressThresholdTime =
    LongPressThresholdTime


modelWith : List Option -> Model t
modelWith options =
    let
        defaultConfig =
            { dragThresholdDistance = 0
            , longPressThresholdTime = 500 * Time.millisecond
            }

        setOption option config =
            case option of
                DragThresholdDistance value ->
                    { config | dragThresholdDistance = value }

                LongPressThresholdTime value ->
                    { config | longPressThresholdTime = value }

        config =
            List.foldl setOption defaultConfig options
    in
    Model
        { config = config
        , mouseState = Resting
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
update message ((Model { config, mouseState, justFinishedDrag }) as model) =
    let
        unexpectedMouseEvent () =
            let
                _ =
                    Debug.log "Unexpected mouse state/message combination"
                        ( mouseState, message )

                _ =
                    Debug.log "Please raise an issue at"
                        "https://github.com/opensolid/svg/issues/new"
            in
            ( model, Nothing )

        setMouseState updatedMouseState =
            Model
                { config = config
                , mouseState = updatedMouseState
                , justFinishedDrag = justFinishedDrag
                }

        finishDrag updatedMouseState =
            Model
                { config = config
                , mouseState = updatedMouseState
                , justFinishedDrag = True
                }
    in
    case ( mouseState, message ) of
        ( Resting, Entered target ) ->
            ( setMouseState (Hovering target), Nothing )

        ( Resting, PrimaryMouseDown { target, point, pageOrigin, modifiers } ) ->
            case target of
                Nothing ->
                    ( setMouseState <|
                        Dragging
                            { target = Nothing
                            , hoverTarget = Nothing
                            , startPoint = point
                            , currentPoint = point
                            , dragStarted = False
                            , pageOrigin = pageOrigin
                            , modifiers = modifiers
                            }
                    , Nothing
                    )

                Just dragTarget ->
                    unexpectedMouseEvent ()

        ( Resting, OtherMouseDown _ ) ->
            ( model, Nothing )

        ( Resting, DraggedTo _ ) ->
            if justFinishedDrag then
                ( Model
                    { config = config
                    , mouseState = mouseState
                    , justFinishedDrag = False
                    }
                , Nothing
                )
            else
                unexpectedMouseEvent ()

        ( Hovering hoverTarget, PrimaryMouseDown { target, point, pageOrigin, modifiers } ) ->
            if target == Just hoverTarget then
                ( setMouseState <|
                    Dragging
                        { target = target
                        , hoverTarget = target
                        , startPoint = point
                        , currentPoint = point
                        , dragStarted = False
                        , pageOrigin = pageOrigin
                        , modifiers = modifiers
                        }
                , Nothing
                )
            else
                unexpectedMouseEvent ()

        ( Hovering _, OtherMouseDown _ ) ->
            ( model, Nothing )

        ( Hovering hoverTarget, Left previousTarget ) ->
            if previousTarget == hoverTarget then
                ( setMouseState Resting, Nothing )
            else
                unexpectedMouseEvent ()

        ( Hovering _, DraggedTo _ ) ->
            if justFinishedDrag then
                ( Model
                    { config = config
                    , mouseState = mouseState
                    , justFinishedDrag = False
                    }
                , Nothing
                )
            else
                unexpectedMouseEvent ()

        ( Dragging properties, Entered hoverTarget ) ->
            if properties.hoverTarget == Nothing then
                ( setMouseState <|
                    Dragging { properties | hoverTarget = Just hoverTarget }
                , Nothing
                )
            else
                unexpectedMouseEvent ()

        ( Dragging properties, Left hoverTarget ) ->
            if properties.hoverTarget == Just hoverTarget then
                ( setMouseState <|
                    Dragging { properties | hoverTarget = Nothing }
                , Nothing
                )
            else
                unexpectedMouseEvent ()

        ( Dragging properties, KeyDown code ) ->
            let
                updatedModifiers =
                    keyDown code properties.modifiers
            in
            ( setMouseState <|
                Dragging { properties | modifiers = updatedModifiers }
            , Nothing
            )

        ( Dragging properties, KeyUp code ) ->
            let
                updatedModifiers =
                    keyUp code properties.modifiers
            in
            ( setMouseState <|
                Dragging { properties | modifiers = updatedModifiers }
            , Nothing
            )

        ( Dragging properties, MouseUp point ) ->
            let
                { target, hoverTarget, startPoint, currentPoint, modifiers, dragStarted } =
                    properties
            in
            if point == currentPoint || not dragStarted then
                let
                    interaction =
                        if dragStarted then
                            Release target modifiers <|
                                { startPoint = startPoint
                                , endPoint = point
                                }
                        else
                            Click target modifiers

                    updatedState =
                        case hoverTarget of
                            Nothing ->
                                Resting

                            Just hoverTarget ->
                                Hovering hoverTarget
                in
                ( finishDrag updatedState, Just interaction )
            else
                unexpectedMouseEvent ()

        ( Dragging properties, OtherMouseDown point ) ->
            let
                { target, hoverTarget, startPoint, currentPoint, modifiers, dragStarted } =
                    properties
            in
            if point == currentPoint || not dragStarted then
                let
                    interaction =
                        if dragStarted then
                            Just <|
                                Release target modifiers <|
                                    { startPoint = startPoint
                                    , endPoint = point
                                    }
                        else
                            Nothing

                    updatedState =
                        case hoverTarget of
                            Nothing ->
                                Resting

                            Just hoverTarget ->
                                Hovering hoverTarget
                in
                ( finishDrag updatedState, interaction )
            else
                unexpectedMouseEvent ()

        ( Dragging properties, DraggedTo newPoint ) ->
            let
                { target, modifiers, startPoint } =
                    properties

                dragStarted =
                    properties.dragStarted
                        || (Point2d.distanceFrom startPoint newPoint
                                > config.dragThresholdDistance
                           )
            in
            if dragStarted then
                ( setMouseState <|
                    Dragging
                        { properties
                            | currentPoint = newPoint
                            , dragStarted = True
                        }
                , Just <|
                    Drag target modifiers <|
                        { startPoint = startPoint
                        , previousPoint = properties.currentPoint
                        , currentPoint = newPoint
                        }
                )
            else
                ( model, Nothing )

        ( _, Tick ) ->
            ( Model
                { config = config
                , mouseState = mouseState
                , justFinishedDrag = False
                }
            , Nothing
            )

        _ ->
            unexpectedMouseEvent ()


subscriptions : Model t -> Sub (Msg t)
subscriptions (Model { mouseState, justFinishedDrag }) =
    case mouseState of
        Dragging { pageOrigin } ->
            let
                ( x0, y0 ) =
                    Point2d.coordinates pageOrigin

                toPoint { x, y } =
                    Point2d.fromCoordinates
                        ( x0 + toFloat x + 0.5
                        , y0 - toFloat y - 0.5
                        )
            in
            Sub.batch
                [ Mouse.moves (toPoint >> DraggedTo)
                , Mouse.ups (toPoint >> MouseUp)

                -- We should only be in a dragging state if the primary button
                -- is down, so any further mouse downs must be non-primary
                -- buttons
                , Mouse.downs (toPoint >> OtherMouseDown)

                -- Track key ups/downs to keep track of modifier state
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


type alias MouseDownEvent =
    { clientX : Float
    , clientY : Float
    , pageX : Float
    , pageY : Float
    , button : Int
    , modifiers : Modifiers
    }


decodeModifiers : Decoder Modifiers
decodeModifiers =
    Decode.map2 Modifiers
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)


decodeMouseDownEvent : Decoder MouseDownEvent
decodeMouseDownEvent =
    Decode.map6 MouseDownEvent
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
        (Decode.field "button" Decode.int)
        decodeModifiers


type alias TouchEvent =
    { clientX : Float
    , clientY : Float
    , identifier : Int
    }


decodeTouchEvent : Decoder TouchEvent
decodeTouchEvent =
    Decode.map3 TouchEvent
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "identifier" Decode.int)


decodeMouseDown : Maybe t -> BoundingBox2d -> Decoder (Msg t)
decodeMouseDown target renderBounds =
    decodeMouseDownEvent
        |> Decode.map
            (\{ clientX, clientY, pageX, pageY, button, modifiers } ->
                let
                    { minX, maxY } =
                        BoundingBox2d.extrema renderBounds

                    x =
                        minX + clientX + 0.5

                    y =
                        maxY - clientY - 0.5

                    point =
                        Point2d.fromCoordinates ( x, y )

                    x0 =
                        minX + clientX - pageX

                    y0 =
                        maxY - clientY + pageY

                    pageOrigin =
                        Point2d.fromCoordinates ( x0, y0 )
                in
                if button == 0 then
                    PrimaryMouseDown
                        { target = target
                        , point = point
                        , pageOrigin = pageOrigin
                        , modifiers = modifiers
                        }
                else
                    OtherMouseDown point
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
isHovering target model =
    hoverTarget model == Just target


isDragging : t -> Model t -> Bool
isDragging target model =
    dragTarget model == Just target


hoverTarget : Model t -> Maybe t
hoverTarget (Model { mouseState }) =
    case mouseState of
        Resting ->
            Nothing

        Hovering target ->
            Just target

        Dragging _ ->
            Nothing


dragTarget : Model t -> Maybe t
dragTarget (Model { mouseState }) =
    case mouseState of
        Resting ->
            Nothing

        Hovering _ ->
            Nothing

        Dragging { target } ->
            target


selectionBox : Model t -> Maybe ( Point2d, Point2d, Modifiers )
selectionBox (Model { mouseState }) =
    case mouseState of
        Resting ->
            Nothing

        Hovering _ ->
            Nothing

        Dragging { target, dragStarted, startPoint, currentPoint, modifiers } ->
            case target of
                Just dragTarget ->
                    Nothing

                Nothing ->
                    if dragStarted then
                        Just ( startPoint, currentPoint, modifiers )
                    else
                        Nothing


dragState : Model t -> Maybe (DragState t)
dragState (Model { mouseState }) =
    case mouseState of
        Resting ->
            Nothing

        Hovering _ ->
            Nothing

        Dragging { target, dragStarted, startPoint, currentPoint, modifiers } ->
            if dragStarted then
                Just <|
                    { target = target
                    , startPoint = startPoint
                    , currentPoint = currentPoint
                    , modifiers = modifiers
                    }
            else
                Nothing
