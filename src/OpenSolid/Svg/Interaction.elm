module OpenSolid.Svg.Interaction
    exposing
        ( Interaction(..)
        , Model
        , Modifiers
        , Msg
        , Option
        , Touch(Touch)
        , container
        , customHandle
        , directionTipHandle
        , dragState
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
        , subscriptions
        , triangleHandle
        , update
        , vectorTipHandle
        )

import AnimationFrame
import DOM
import Dict exposing (Dict)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Keyboard
import Mouse
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Svg.Interaction.ScrollAmount as ScrollAmount exposing (ScrollAmount)
import OpenSolid.Svg.Internal as Internal
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes
import Time exposing (Time)


type alias Modifiers =
    { ctrl : Bool
    , shift : Bool
    }


type alias HoverState t =
    { target : Maybe t
    , container : Maybe t
    }


type MouseState t
    = Resting (HoverState t)
    | Dragging
        { target : t
        , hoverState : HoverState t
        , startPoint : Point2d
        , dragStarted : Bool
        , currentPoint : Point2d
        , pageOrigin : Point2d
        , modifiers : Modifiers
        }


type TouchProgress
    = Tapping Float
    | LongPressed
    | Gesturing


type alias ActiveTouch t =
    { target : t
    , elapsedTime : Time
    , startPoint : Point2d
    , currentPoint : Point2d
    , progress : TouchProgress
    }


type alias Config =
    { dragThresholdDistance : Float
    , longPressThresholdTime : Time
    }


type Model t
    = Model
        { config : Config
        , mouseState : MouseState t
        , justFinishedDrag : Bool
        , touchState : Dict Int (ActiveTouch t)
        }


type Option
    = DragThresholdDistance Float
    | LongPressThresholdTime Time


type MouseMsg t
    = EnteredTarget t
    | LeftTarget t
    | EnteredContainer t
    | LeftContainer t
    | PrimaryMouseDown
        { target : t
        , point : Point2d
        , pageOrigin : Point2d
        , modifiers : Modifiers
        }
    | OtherMouseDown Point2d
    | DraggedTo Point2d
    | MouseUp Point2d
    | Scrolled t Modifiers ScrollAmount
    | KeyDown Int
    | KeyUp Int


type alias TouchEvent t =
    { target : t
    , identifier : Int
    , point : Point2d
    }


type TouchMsg t
    = TouchStart (List (TouchEvent t))
    | TouchMove (List (TouchEvent t))
    | TouchEnd (List (TouchEvent t))
    | UpdateTouchProgress Time


type Msg t
    = MouseMsg (MouseMsg t)
    | TouchMsg (TouchMsg t)
    | Tick Time


type Touch t
    = Touch t { identifier : Int, startPoint : Point2d, previousPoint : Point2d, currentPoint : Point2d }


type Interaction t
    = Click t Modifiers
    | Drag t Modifiers { startPoint : Point2d, previousPoint : Point2d, currentPoint : Point2d }
    | Release t Modifiers { startPoint : Point2d, endPoint : Point2d }
    | Scroll t Modifiers ScrollAmount
    | Tap t
    | LongPress t
    | Slide t { startPoint : Point2d, previousPoint : Point2d, currentPoint : Point2d }
    | Lift t


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
        , mouseState = Resting { target = Nothing, container = Nothing }
        , justFinishedDrag = False
        , touchState = Dict.empty
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


eventOptions : { preventDefault : Bool, stopPropagation : Bool }
eventOptions =
    { preventDefault = True
    , stopPropagation = True
    }


on : String -> Decoder msg -> Svg.Attribute msg
on eventName decoder =
    Html.Events.onWithOptions eventName eventOptions decoder


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


issueMessage : String
issueMessage =
    "Please raise an issue at https://github.com/opensolid/svg/issues/new"


logError : x -> a -> a
logError error returnValue =
    let
        _ =
            Debug.log issueMessage error
    in
    returnValue


enterContainer : t -> HoverState t -> HoverState t
enterContainer container hoverState =
    case hoverState.container of
        Nothing ->
            { hoverState | container = Just container }

        Just currentContainer ->
            if container == currentContainer then
                hoverState
            else
                { hoverState | container = Just container }
                    |> logError
                        ("Entered container "
                            ++ toString container
                            ++ " while already in container "
                            ++ toString currentContainer
                        )


leaveContainer : t -> HoverState t -> HoverState t
leaveContainer container hoverState =
    case hoverState.container of
        Just currentContainer ->
            if container == currentContainer then
                case hoverState.target of
                    Nothing ->
                        { hoverState | container = Nothing }

                    Just currentTarget ->
                        { hoverState | container = Nothing }
                            |> logError
                                ("Left container "
                                    ++ toString container
                                    ++ " while still over target "
                                    ++ toString currentTarget
                                )
            else
                hoverState
                    |> logError
                        ("Left container "
                            ++ toString container
                            ++ " while in container "
                            ++ toString currentContainer
                        )

        Nothing ->
            hoverState
                |> logError
                    ("Left container "
                        ++ toString container
                        ++ " while already outside container"
                    )


enterTarget : t -> HoverState t -> HoverState t
enterTarget target hoverState =
    case hoverState.target of
        Nothing ->
            { hoverState | target = Just target }

        Just currentTarget ->
            if currentTarget == target then
                hoverState
                    |> logError ("Re-entered target " ++ toString currentTarget)
            else
                { hoverState | target = Just target }
                    |> logError
                        ("Entered target "
                            ++ toString target
                            ++ " while still over target "
                            ++ toString currentTarget
                        )


leaveTarget : t -> HoverState t -> HoverState t
leaveTarget target hoverState =
    case hoverState.target of
        Just currentTarget ->
            if currentTarget == target then
                { hoverState | target = Nothing }
            else
                hoverState
                    |> Debug.log
                        ("Left target"
                            ++ toString target
                            ++ " while over target "
                            ++ toString currentTarget
                        )

        Nothing ->
            hoverState
                |> Debug.log
                    ("Left target "
                        ++ toString target
                        ++ " while not over any target"
                    )


handleMouseMessage : MouseMsg t -> Model t -> ( Model t, List (Interaction t) )
handleMouseMessage message ((Model modelProperties) as model) =
    let
        { config, mouseState, justFinishedDrag, touchState } =
            modelProperties

        unexpectedMouseEvent () =
            let
                _ =
                    Debug.log "Unexpected mouse state/message combination"
                        ( mouseState, message )

                _ =
                    Debug.log "Please raise an issue at"
                        "https://github.com/opensolid/svg/issues/new"
            in
            ( model, [] )

        setMouseState updatedMouseState =
            Model
                { config = config
                , mouseState = updatedMouseState
                , justFinishedDrag = justFinishedDrag
                , touchState = touchState
                }

        finishDrag updatedMouseState =
            Model
                { config = config
                , mouseState = updatedMouseState
                , justFinishedDrag = True
                , touchState = touchState
                }
    in
    case ( mouseState, message ) of
        ( Resting hoverState, EnteredTarget target ) ->
            ( setMouseState (Resting (enterTarget target hoverState))
            , []
            )

        ( Resting hoverState, LeftTarget target ) ->
            ( setMouseState (Resting (leaveTarget target hoverState))
            , []
            )

        ( Resting hoverState, EnteredContainer container ) ->
            ( setMouseState (Resting (enterContainer container hoverState))
            , []
            )

        ( Resting hoverState, LeftContainer container ) ->
            ( setMouseState (Resting (leaveContainer container hoverState))
            , []
            )

        ( Dragging properties, EnteredTarget target ) ->
            ( setMouseState <|
                Dragging
                    { properties
                        | hoverState = enterTarget target properties.hoverState
                    }
            , []
            )

        ( Dragging properties, LeftTarget target ) ->
            ( setMouseState <|
                Dragging
                    { properties
                        | hoverState = leaveTarget target properties.hoverState
                    }
            , []
            )

        ( Dragging properties, EnteredContainer container ) ->
            ( setMouseState <|
                Dragging
                    { properties
                        | hoverState =
                            enterContainer container properties.hoverState
                    }
            , []
            )

        ( Dragging properties, LeftContainer container ) ->
            ( setMouseState <|
                Dragging
                    { properties
                        | hoverState =
                            leaveContainer container properties.hoverState
                    }
            , []
            )

        ( Resting hoverState, PrimaryMouseDown mouseDown ) ->
            let
                startDrag () =
                    ( setMouseState <|
                        Dragging
                            { target = mouseDown.target
                            , hoverState = hoverState
                            , startPoint = mouseDown.point
                            , currentPoint = mouseDown.point
                            , dragStarted = False
                            , pageOrigin = mouseDown.pageOrigin
                            , modifiers = mouseDown.modifiers
                            }
                    , []
                    )
            in
            case ( hoverState.target, hoverState.container ) of
                ( Just hoverTarget, _ ) ->
                    if mouseDown.target == hoverTarget then
                        startDrag ()
                    else
                        unexpectedMouseEvent ()

                ( Nothing, Just hoverContainer ) ->
                    if mouseDown.target == hoverContainer then
                        startDrag ()
                    else
                        unexpectedMouseEvent ()

                ( Nothing, Nothing ) ->
                    unexpectedMouseEvent ()

        ( Resting _, OtherMouseDown _ ) ->
            ( model, [] )

        ( Resting _, DraggedTo _ ) ->
            if justFinishedDrag then
                ( Model
                    { config = config
                    , mouseState = mouseState
                    , justFinishedDrag = False
                    , touchState = touchState
                    }
                , []
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
            , []
            )

        ( Dragging properties, KeyUp code ) ->
            let
                updatedModifiers =
                    keyUp code properties.modifiers
            in
            ( setMouseState <|
                Dragging { properties | modifiers = updatedModifiers }
            , []
            )

        ( Dragging properties, MouseUp point ) ->
            let
                { target, hoverState, startPoint, currentPoint, modifiers, dragStarted } =
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
                in
                ( finishDrag (Resting hoverState), [ interaction ] )
            else
                unexpectedMouseEvent ()

        ( Dragging properties, OtherMouseDown point ) ->
            let
                { target, hoverState, startPoint, currentPoint, modifiers, dragStarted } =
                    properties
            in
            if point == currentPoint || not dragStarted then
                let
                    interaction =
                        if dragStarted then
                            [ Release target modifiers <|
                                { startPoint = startPoint
                                , endPoint = point
                                }
                            ]
                        else
                            []
                in
                ( finishDrag (Resting hoverState), interaction )
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
                , [ Drag target modifiers <|
                        { startPoint = startPoint
                        , previousPoint = properties.currentPoint
                        , currentPoint = newPoint
                        }
                  ]
                )
            else
                ( model, [] )

        ( _, Scrolled target modifiers scrollAmount ) ->
            ( model, [ Scroll target modifiers scrollAmount ] )

        _ ->
            unexpectedMouseEvent ()


finalizeDrag : Model t -> Model t
finalizeDrag (Model properties) =
    Model { properties | justFinishedDrag = False }


handleTouchStart : List (TouchEvent t) -> Model t -> ( Model t, List (Interaction t) )
handleTouchStart touchEvents (Model properties) =
    let
        startTouch touchEvent =
            Dict.update touchEvent.identifier
                (\currentEntry ->
                    case currentEntry of
                        Nothing ->
                            Just
                                { target = touchEvent.target
                                , elapsedTime = 0
                                , startPoint = touchEvent.point
                                , currentPoint = touchEvent.point
                                , progress = Tapping 0
                                }

                        Just activeTouch ->
                            currentEntry
                                |> logError
                                    ("Started touch for already-active touch "
                                        ++ toString activeTouch
                                    )
                )

        updatedTouchState =
            List.foldl startTouch properties.touchState touchEvents
    in
    ( Model { properties | touchState = updatedTouchState }, [] )


moveTouch : Config -> TouchEvent t -> ActiveTouch t -> ( ActiveTouch t, List (Interaction t) )
moveTouch { dragThresholdDistance } touchEvent activeTouch =
    let
        updatedTouch =
            { activeTouch | currentPoint = touchEvent.point }

        checkForGestureStart () =
            let
                distanceFromStart =
                    Point2d.distanceFrom activeTouch.startPoint touchEvent.point
            in
            if distanceFromStart > dragThresholdDistance then
                ( { updatedTouch | progress = Gesturing }
                , [ Slide touchEvent.target
                        { startPoint = activeTouch.startPoint
                        , previousPoint = activeTouch.startPoint
                        , currentPoint = touchEvent.point
                        }
                  ]
                )
            else
                ( updatedTouch, [] )
    in
    case activeTouch.progress of
        Gesturing ->
            ( updatedTouch
            , [ Slide touchEvent.target
                    { startPoint = activeTouch.startPoint
                    , previousPoint = activeTouch.currentPoint
                    , currentPoint = touchEvent.point
                    }
              ]
            )

        Tapping elapsedTime ->
            checkForGestureStart ()

        LongPressed ->
            checkForGestureStart ()


handleTouchMove : List (TouchEvent t) -> Model t -> ( Model t, List (Interaction t) )
handleTouchMove touchEvents (Model properties) =
    let
        processEvent touchEvent ( touchState, accumulatedInteractions ) =
            case Dict.get touchEvent.identifier touchState of
                Just activeTouch ->
                    if touchEvent.target == activeTouch.target then
                        let
                            ( updatedTouch, newInteractions ) =
                                moveTouch properties.config
                                    touchEvent
                                    activeTouch

                            updatedTouchState =
                                touchState
                                    |> Dict.insert touchEvent.identifier
                                        updatedTouch
                        in
                        ( updatedTouchState
                        , accumulatedInteractions ++ newInteractions
                        )
                    else
                        ( touchState, accumulatedInteractions )
                            |> logError
                                ("Touch move event "
                                    ++ toString touchEvent
                                    ++ " does not match active touch "
                                    ++ toString activeTouch
                                )

                Nothing ->
                    ( touchState, accumulatedInteractions )
                        |> logError
                            ("No active touch found for touch move event "
                                ++ toString touchEvent
                            )

        ( updatedTouchState, interactions ) =
            List.foldl processEvent ( properties.touchState, [] ) touchEvents
    in
    ( Model { properties | touchState = updatedTouchState }
    , interactions
    )


handleTouchEnd : List (TouchEvent t) -> Model t -> ( Model t, List (Interaction t) )
handleTouchEnd touchEvents (Model properties) =
    let
        processEvent touchEvent ( touchState, accumulatedTaps, accumulatedLifts ) =
            case Dict.get touchEvent.identifier touchState of
                Just activeTouch ->
                    if touchEvent.target == activeTouch.target then
                        let
                            updatedTouchState =
                                touchState |> Dict.remove touchEvent.identifier
                        in
                        case activeTouch.progress of
                            Tapping _ ->
                                ( updatedTouchState
                                , activeTouch.target :: accumulatedTaps
                                , accumulatedLifts
                                )

                            LongPressed ->
                                ( updatedTouchState
                                , accumulatedTaps
                                , accumulatedLifts
                                )

                            Gesturing ->
                                ( updatedTouchState
                                , accumulatedTaps
                                , activeTouch.target :: accumulatedLifts
                                )
                    else
                        ( touchState, accumulatedTaps, accumulatedLifts )
                            |> logError
                                ("Touch end event "
                                    ++ toString touchEvent
                                    ++ " does not match active touch "
                                    ++ toString activeTouch
                                )

                Nothing ->
                    ( touchState, accumulatedTaps, accumulatedLifts )
                        |> logError
                            ("No active touch found for touch end event "
                                ++ toString touchEvent
                            )

        ( updatedTouchState, tapTargets, liftTargets ) =
            List.foldl processEvent
                ( properties.touchState, [], [] )
                touchEvents

        interactions =
            List.map Tap (List.reverse tapTargets)
                ++ List.map Lift (List.reverse liftTargets)
    in
    ( Model { properties | touchState = updatedTouchState }
    , interactions
    )


updateTouchProgress : Float -> Model t -> ( Model t, List (Interaction t) )
updateTouchProgress delta (Model properties) =
    let
        { longPressThresholdTime } =
            properties.config

        processActiveTouch identifier activeTouch ( touchState, targets ) =
            case activeTouch.progress of
                Tapping elapsedTime ->
                    let
                        updatedTime =
                            elapsedTime + delta

                        ( updatedProgress, pressedTargets ) =
                            if updatedTime > longPressThresholdTime then
                                ( LongPressed, [ activeTouch.target ] )
                            else
                                ( Tapping updatedTime, [] )

                        updatedTouch =
                            { activeTouch | progress = updatedProgress }

                        updatedTouchState =
                            touchState
                                |> Dict.insert identifier updatedTouch
                    in
                    ( touchState |> Dict.insert identifier updatedTouch
                    , targets ++ pressedTargets
                    )

                _ ->
                    ( touchState |> Dict.insert identifier activeTouch
                    , targets
                    )

        ( updatedTouchState, targets ) =
            properties.touchState
                |> Dict.foldl processActiveTouch ( Dict.empty, [] )

        interactions =
            if List.isEmpty targets then
                []
            else
                List.map LongPress targets
    in
    ( Model { properties | touchState = updatedTouchState }
    , interactions
    )


handleTouchMessage : TouchMsg t -> Model t -> ( Model t, List (Interaction t) )
handleTouchMessage touchMessage model =
    case touchMessage of
        TouchStart touchEvents ->
            handleTouchStart touchEvents model

        TouchMove touchEvents ->
            handleTouchMove touchEvents model

        TouchEnd touchEvents ->
            handleTouchEnd touchEvents model

        UpdateTouchProgress delta ->
            updateTouchProgress delta model


update : Msg t -> Model t -> ( Model t, List (Interaction t) )
update message model =
    case message of
        MouseMsg mouseMessage ->
            model |> handleMouseMessage mouseMessage

        TouchMsg touchMessage ->
            model |> handleTouchMessage touchMessage

        Tick delta ->
            model
                |> finalizeDrag
                |> handleTouchMessage (UpdateTouchProgress delta)


mouseSubscriptions : Model t -> Sub (Msg t)
mouseSubscriptions (Model { mouseState, justFinishedDrag }) =
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
                [ Mouse.moves (toPoint >> DraggedTo >> MouseMsg)
                , Mouse.ups (toPoint >> MouseUp >> MouseMsg)

                -- We should only be in a dragging state if the primary button
                -- is down, so any further mouse downs must be non-primary
                -- buttons
                , Mouse.downs (toPoint >> OtherMouseDown >> MouseMsg)

                -- Track key ups/downs to keep track of modifier state
                , Keyboard.ups (KeyUp >> MouseMsg)
                , Keyboard.downs (KeyDown >> MouseMsg)
                ]

        _ ->
            if justFinishedDrag then
                AnimationFrame.diffs Tick
            else
                Sub.none


isTapping : ActiveTouch t -> Bool
isTapping activeTouch =
    case activeTouch.progress of
        Tapping _ ->
            True

        _ ->
            False


touchSubscriptions : Model t -> Sub (Msg t)
touchSubscriptions (Model { touchState }) =
    let
        activeTouches =
            Dict.values touchState
    in
    if List.any isTapping activeTouches then
        AnimationFrame.diffs Tick
    else
        Sub.none


subscriptions : Model t -> Sub (Msg t)
subscriptions model =
    Sub.batch [ mouseSubscriptions model, touchSubscriptions model ]


container : (Msg t -> msg) -> { target : t, renderBounds : BoundingBox2d } -> List (Svg msg) -> Svg msg
container tagger { target, renderBounds } children =
    let
        attributes =
            [ on "mousedown"
                (decodeMouseDown target |> Decode.map tagger)
            , on "mouseenter"
                (Decode.succeed (tagger (MouseMsg (EnteredContainer target))))
            , on "mouseleave"
                (Decode.succeed (tagger (MouseMsg (LeftContainer target))))
            , on "touchstart"
                (Decode.map (TouchStart >> TouchMsg >> tagger) (decodeTouchEvents target))
            , on "touchmove"
                (Decode.map (TouchMove >> TouchMsg >> tagger) (decodeTouchEvents target))
            , on "touchend"
                (Decode.map (TouchEnd >> TouchMsg >> tagger) (decodeTouchEvents target))
            , on "wheel"
                (decodeWheel target |> Decode.map tagger)
            ]

        background =
            Svg.boundingBox2d [ transparentFill, noStroke ] renderBounds
    in
    Svg.g attributes (background :: children)


type alias MouseDownProperties =
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


decodeBoundingClientRect : Decoder DOM.Rectangle
decodeBoundingClientRect =
    Decode.at [ "target", "ownerSVGElement", "parentNode" ]
        DOM.boundingClientRect


decodeMouseDownProperties : Decoder MouseDownProperties
decodeMouseDownProperties =
    Decode.map6 MouseDownProperties
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
        (Decode.field "button" Decode.int)
        decodeModifiers


type alias TouchProperties =
    { clientX : Float
    , clientY : Float
    , identifier : Int
    , boundingClientRect : DOM.Rectangle
    }


decodeTouchProperties : Decoder (List TouchProperties)
decodeTouchProperties =
    let
        decodeSingle : Decoder TouchProperties
        decodeSingle =
            Decode.map4 TouchProperties
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
                (Decode.field "identifier" Decode.int)
                decodeBoundingClientRect

        collect : List TouchProperties -> Int -> Decoder (List TouchProperties)
        collect accumulated count =
            if count > 0 then
                Decode.field (toString (count - 1)) decodeSingle
                    |> Decode.andThen
                        (\decoded ->
                            collect (decoded :: accumulated) (count - 1)
                        )
            else
                Decode.succeed accumulated
    in
    Decode.field "changedTouches"
        (Decode.field "length" Decode.int |> Decode.andThen (collect []))


decodeMouseDown : t -> Decoder (Msg t)
decodeMouseDown target =
    Decode.map2
        (\mouseDownProperties boundingClientRect ->
            let
                { clientX, clientY, pageX, pageY, button, modifiers } =
                    mouseDownProperties

                { left, top, height } =
                    boundingClientRect

                x =
                    clientX - left + 0.5

                y =
                    top + height - clientY - 0.5

                point =
                    Point2d.fromCoordinates ( x, y )

                x0 =
                    -left

                y0 =
                    top + height

                pageOrigin =
                    Point2d.fromCoordinates ( x0, y0 )
            in
            if button == 0 then
                MouseMsg <|
                    PrimaryMouseDown
                        { target = target
                        , point = point
                        , pageOrigin = pageOrigin
                        , modifiers = modifiers
                        }
            else
                MouseMsg (OtherMouseDown point)
        )
        decodeMouseDownProperties
        decodeBoundingClientRect


decodeScrollAmount : Decoder ScrollAmount
decodeScrollAmount =
    Decode.map2 (,)
        (Decode.field "deltaMode" Decode.int)
        (Decode.field "deltaY" Decode.float)
        |> Decode.andThen
            (\( deltaMode, value ) ->
                case deltaMode of
                    0 ->
                        Decode.succeed (ScrollAmount.Pixels value)

                    1 ->
                        Decode.succeed (ScrollAmount.Lines value)

                    2 ->
                        Decode.succeed (ScrollAmount.Pages value)

                    _ ->
                        Decode.fail "Unrecognized delta mode"
            )


decodeWheel : t -> Decoder (Msg t)
decodeWheel target =
    Decode.map2 (Scrolled target)
        decodeModifiers
        decodeScrollAmount
        |> Decode.map MouseMsg


decodeTouchEvents : t -> Decoder (List (TouchEvent t))
decodeTouchEvents target =
    decodeTouchProperties
        |> Decode.map
            (List.map
                (\{ identifier, clientX, clientY, boundingClientRect } ->
                    let
                        { left, top, height } =
                            boundingClientRect

                        x =
                            clientX - left + 0.5

                        y =
                            top + height - clientY - 0.5

                        point =
                            Point2d.fromCoordinates ( x, y )
                    in
                    { target = target
                    , identifier = identifier
                    , point = point
                    }
                )
            )


customHandle : Svg Never -> t -> Svg (Msg t)
customHandle shape target =
    let
        attributes =
            [ on "mousedown" (decodeMouseDown target)
            , on "mouseenter" (Decode.succeed (MouseMsg (EnteredTarget target)))
            , on "mouseleave" (Decode.succeed (MouseMsg (LeftTarget target)))
            , on "touchstart" (Decode.map (TouchStart >> TouchMsg) (decodeTouchEvents target))
            , on "touchmove" (Decode.map (TouchMove >> TouchMsg) (decodeTouchEvents target))
            , on "touchend" (Decode.map (TouchEnd >> TouchMsg) (decodeTouchEvents target))
            , on "wheel" (decodeWheel target)
            ]
    in
    Svg.g attributes [ shape |> Svg.map never ]


transparentFill : Svg.Attribute msg
transparentFill =
    Svg.Attributes.fill "transparent"


noStroke : Svg.Attribute msg
noStroke =
    Svg.Attributes.stroke "none"


pointHandle : Point2d -> { target : t, radius : Float } -> Svg (Msg t)
pointHandle point { target, radius } =
    let
        shape =
            Svg.point2dWith { radius = radius }
                [ noStroke, transparentFill ]
                point
    in
    customHandle shape target


thickened : Float -> List (Svg.Attribute msg)
thickened padding =
    [ Svg.Attributes.fill "transparent"
    , Svg.Attributes.stroke "transparent"
    , Svg.Attributes.strokeWidth (toString (2 * padding))
    , Svg.Attributes.strokeLinejoin "round"
    , Svg.Attributes.strokeLinecap "round"
    ]


lineSegmentHandle : LineSegment2d -> { target : t, padding : Float } -> Svg (Msg t)
lineSegmentHandle lineSegment { target, padding } =
    let
        shape =
            Svg.lineSegment2d (thickened padding) lineSegment
    in
    customHandle shape target


triangleHandle : Triangle2d -> { target : t, padding : Float } -> Svg (Msg t)
triangleHandle triangle { target, padding } =
    let
        shape =
            Svg.triangle2d (thickened padding) triangle
    in
    customHandle shape target


vectorTipHandle : Point2d -> Vector2d -> { target : t, tipLength : Float, tipWidth : Float, padding : Float } -> Svg (Msg t)
vectorTipHandle basePoint vector { target, tipLength, tipWidth, padding } =
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
            customHandle shape target

        Nothing ->
            pointHandle basePoint
                { target = target
                , radius = padding
                }


directionTipHandle : Point2d -> Direction2d -> { length : Float, tipLength : Float, tipWidth : Float, target : t, padding : Float } -> Svg (Msg t)
directionTipHandle basePoint direction { length, tipLength, tipWidth, padding, target } =
    let
        vector =
            Vector2d.with { length = length, direction = direction }
    in
    vectorTipHandle basePoint vector <|
        { target = target
        , tipLength = tipLength
        , tipWidth = tipWidth
        , padding = padding
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
        Resting hoverState ->
            hoverState.target

        Dragging _ ->
            Nothing


dragTarget : Model t -> Maybe t
dragTarget (Model { mouseState }) =
    case mouseState of
        Resting _ ->
            Nothing

        Dragging { target } ->
            Just target


dragState : Model t -> Maybe { target : t, startPoint : Point2d, currentPoint : Point2d, modifiers : Modifiers }
dragState (Model { mouseState }) =
    case mouseState of
        Resting _ ->
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
