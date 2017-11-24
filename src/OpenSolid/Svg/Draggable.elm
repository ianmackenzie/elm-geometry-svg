module OpenSolid.Svg.Draggable
    exposing
        ( DragHandler
        , Draggable
        , currentValue
        , direction2d
        , point2d
        , render
        , vector2d
        )

import Html.Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


type Draggable a t
    = Draggable
        { currentValue : a
        , dragState : Maybe ( t, Point2d )
        }


type alias Config a t =
    { draw : a -> Svg (DragTarget a t)
    , transform : t -> Point2d -> Point2d -> a -> a
    }


type Target t
    = DragStart t ( Float, Float )


target : t -> Svg.Attribute (Target t)
target dragTarget =
    Svg.Events.on "mousedown" <|
        Decode.map (DragStart dragTarget) decodeClientPos


currentValue : Draggable a -> a
currentValue (Draggable { currentValue }) =
    currentValue


decodeClientPos : Decoder ( Float, Float )
decodeClientPos =
    Decode.map2 (,)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


toPoint : BoundingBox2d -> ( Float, Float ) -> Point2d
toPoint boundingBox ( clientX, clientY ) =
    Point2d.fromCoordinates
        ( BoundingBox2d.minX boundingBox + clientX
        , BoundingBox2d.maxY boundingBox - clientY
        )


decodePoint : BoundingBox2d -> Decoder Point2d
decodePoint boundingBox =
    Decode.map (toPoint boundingBox) decodeClientPos


render : BoundingBox2d -> Config a t -> Draggable a t -> Svg (Draggable a t)
render boundingBox { draw, transform } (Draggable properties) =
    let
        { currentValue, dragState } =
            properties
    in
    case dragState of
        Just ( currentPoint, dragTarget ) ->
            let
                dragAttributes =
                    [ Svg.Attributes.fill "transparent"
                    , Svg.Attributes.stroke "lightgrey"
                    , Svg.Attributes.strokeDasharray "5 5"
                    , Svg.Events.on "mousemove"
                        (decodePoint boundingBox
                            |> Decode.map
                                (\newPoint ->
                                    let
                                        updatedValue =
                                            transform
                                                dragTarget
                                                currentPoint
                                                newPoint
                                                currentValue
                                    in
                                    Draggable
                                        { properties
                                            | currentValue = updatedValue
                                            , dragState =
                                                Just ( dragTarget, newPoint )
                                        }
                                )
                        )
                    , Svg.Events.onMouseUp <|
                        Draggable { properties | dragState = Nothing }
                    ]

                currentSvg =
                    draw currentValue
                        |> Svg.map (always (Draggable properties))

                dragCircle =
                    Svg.point2d { radius = 100, attributes = dragAttributes }
                        currentPoint
            in
            Svg.g [] [ currentSvg, dragCircle ]

        Nothing ->
            draw currentValue
                |> Svg.map
                    (\(DragStart dragTarget clientPos) ->
                        Draggable
                            { properties
                                | dragState = Just ( dragTarget, toPoint boundingBox clientPos )
                            }
                    )


init : a -> Draggable a
init value =
    Draggable
        { currentValue = value
        , dragState = Nothing
        }


staticAttributes : List (Svg.Attribute Never) -> List (Svg.Attribute a)
staticAttributes =
    List.map (Html.Attributes.map never)


point2d : Svg.PointOptions Never -> Point2d -> Draggable Point2d
point2d initialOptions point =
    let
        transform startPoint endPoint =
            Point2d.translateBy (Vector2d.from startPoint endPoint)

        options =
            { initialOptions
                | attributes =
                    dragHandler transform
                        :: staticAttributes initialOptions.attributes
            }
    in
    init point (Svg.point2d options)


direction2d : Svg.DirectionOptions Never -> Point2d -> Direction2d -> Draggable Direction2d
direction2d initialOptions basePoint direction =
    let
        transform startPoint endPoint currentDirection =
            let
                startDirection =
                    Direction2d.from basePoint startPoint

                endDirection =
                    Direction2d.from basePoint endPoint

                angle =
                    Maybe.map2 Direction2d.angleFrom
                        startDirection
                        endDirection
                        |> Maybe.withDefault 0
            in
            Direction2d.rotateBy angle currentDirection

        options =
            { initialOptions
                | tipAttributes =
                    dragHandler transform
                        :: staticAttributes initialOptions.tipAttributes
                , stemAttributes =
                    staticAttributes initialOptions.stemAttributes
                , groupAttributes =
                    staticAttributes initialOptions.groupAttributes
            }
    in
    init direction (Svg.direction2d options basePoint)


vector2d : Svg.VectorOptions Never -> Point2d -> Vector2d -> Draggable Vector2d
vector2d initialOptions basePoint vector =
    let
        transform startPoint endPoint currentVector =
            Vector2d.sum currentVector (Vector2d.from startPoint endPoint)

        options =
            { initialOptions
                | tipAttributes =
                    dragHandler transform
                        :: staticAttributes initialOptions.tipAttributes
                , stemAttributes =
                    staticAttributes initialOptions.stemAttributes
                , groupAttributes =
                    staticAttributes initialOptions.groupAttributes
            }
    in
    init vector (Svg.vector2d options basePoint)
