module Logo exposing (Model, Msg(..), init, logo, main, update, view)

import Angle exposing (Angle)
import Browser
import Color
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Frame2d
import Frame3d
import Geometry.Svg as Svg
import Html exposing (Html)
import Point2d
import Point3d
import Polygon2d
import Quantity
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Paint(..))


type alias Model =
    { height : Float
    , xOffset : Float
    , yOffset : Float
    , zOffset : Float
    , azimuth : Angle
    , elevation : Angle
    }


init : Model
init =
    { height = 0.8
    , xOffset = 0.6
    , yOffset = 0.6
    , zOffset = 0.6
    , azimuth = Angle.degrees 65
    , elevation = Angle.degrees 20
    }


type Msg
    = HeightInput Float
    | XOffsetInput Float
    | YOffsetInput Float
    | ZOffsetInput Float
    | AzimuthInput Float
    | ElevationInput Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        HeightInput input ->
            { model
                | height = input
            }

        XOffsetInput input ->
            { model
                | xOffset = input
            }

        YOffsetInput input ->
            { model
                | yOffset = input
            }

        ZOffsetInput input ->
            { model
                | zOffset = input
            }

        AzimuthInput input ->
            { model
                | azimuth = Angle.degrees input
            }

        ElevationInput input ->
            { model
                | elevation = Angle.degrees input
            }


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.spacing 13
            , Element.padding 10
            , Element.centerX
            ]
            [ mySlider
                { label = "Height:"
                , input = model.height
                , msg = HeightInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "X Offset: "
                , input = model.xOffset
                , msg = XOffsetInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "Y Offset: "
                , input = model.yOffset
                , msg = YOffsetInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "Z Offset: "
                , input = model.zOffset
                , msg = ZOffsetInput
                , min = 0
                , max = 1
                }
            , mySlider
                { label = "Azimuth: "
                , input = Angle.inDegrees model.azimuth
                , msg = AzimuthInput
                , min = 0
                , max = 90
                }
            , mySlider
                { label = "Elevation: "
                , input = Angle.inDegrees model.elevation
                , msg = ElevationInput
                , min = 0
                , max = 90
                }
            , Element.html <| logo model
            ]


grey : Element.Color
grey =
    Element.rgb 0.5 0.5 0.5


type alias MySliderInput =
    { label : String
    , input : Float
    , msg : Float -> Msg
    , min : Float
    , max : Float
    }


mySlider : MySliderInput -> Element Msg
mySlider input =
    Element.el [ Element.padding 2, Element.width Element.fill ] <|
        Input.slider
            [ Element.behindContent <|
                Element.el
                    [ Element.width Element.fill
                    , Element.height <| Element.px 2
                    , Element.centerY
                    , Background.color grey
                    ]
                    Element.none
            ]
            { onChange = input.msg
            , label =
                Input.labelAbove
                    [ Font.color grey, Element.centerX ]
                <|
                    Element.text input.label
            , min = input.min
            , max = input.max
            , value = input.input
            , thumb = Input.defaultThumb
            , step = Nothing
            }


logo : Model -> Html Msg
logo model =
    let
        p1 =
            Point3d.unitless 1 0 0

        p2 =
            Point3d.unitless 1 1 0

        p3 =
            Point3d.unitless 0 1 0

        p4 =
            Point3d.unitless 0 1 model.height

        p5 =
            Point3d.unitless 0 0 model.height

        p6 =
            Point3d.unitless 1 0 model.height

        p7 =
            Point3d.unitless 1 (1 - model.yOffset) model.height

        p8 =
            Point3d.unitless 1 1 (model.height - model.zOffset)

        p9 =
            Point3d.unitless (1 - model.xOffset) 1 model.height

        eyePoint =
            Point3d.unitless 0.5 0.5 (model.height / 2)

        viewFrame =
            Frame3d.atPoint eyePoint
                |> Frame3d.rotateAroundOwn Frame3d.zAxis model.azimuth
                |> Frame3d.rotateAroundOwn Frame3d.yAxis (Quantity.negate model.elevation)

        to2d =
            Point3d.projectInto (Frame3d.yzSketchPlane viewFrame)

        leftPolygon =
            Polygon2d.singleLoop (List.map to2d [ p1, p2, p8, p7, p6 ])

        rightPolygon =
            Polygon2d.singleLoop (List.map to2d [ p2, p3, p4, p9, p8 ])

        topPolygon =
            Polygon2d.singleLoop (List.map to2d [ p6, p7, p9, p4, p5 ])

        trianglePolygon =
            Polygon2d.singleLoop (List.map to2d [ p7, p8, p9 ])

        orange =
            Color.rgb255 240 173 0

        green =
            Color.rgb255 127 209 59

        lightBlue =
            Color.rgb255 96 181 204

        darkBlue =
            Color.rgb255 90 99 120

        mask id polygon =
            let
                attributes =
                    [ TypedSvg.Attributes.fill <| Paint Color.white
                    , TypedSvg.Attributes.stroke <| Paint Color.black
                    , TypedSvg.Attributes.InPx.strokeWidth 0.03
                    ]
            in
            TypedSvg.mask [ TypedSvg.Attributes.id id ]
                [ Svg.polygon2d attributes polygon ]

        face color clipPathId polygon =
            let
                attributes =
                    [ TypedSvg.Attributes.fill <| Paint color
                    , TypedSvg.Attributes.mask ("url(#" ++ clipPathId ++ ")")
                    ]
            in
            Svg.polygon2d attributes polygon

        defs =
            TypedSvg.defs []
                [ mask "leftOutline" leftPolygon
                , mask "rightOutline" rightPolygon
                , mask "topOutline" topPolygon
                , mask "triangleOutline" trianglePolygon
                ]

        leftFace =
            face orange "leftOutline" leftPolygon

        rightFace =
            face lightBlue "rightOutline" rightPolygon

        topFace =
            face green "topOutline" topPolygon

        triangleFace =
            face darkBlue "triangleOutline" trianglePolygon

        elements =
            TypedSvg.g [] [ defs, leftFace, rightFace, topFace, triangleFace ]

        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels -250 250)
                |> Frame2d.reverseY

        scene =
            Svg.relativeTo topLeftFrame
                (Svg.scaleAbout Point2d.origin 200 elements)
    in
    TypedSvg.svg
        [ TypedSvg.Attributes.InPx.width 500
        , TypedSvg.Attributes.InPx.height 500
        ]
        [ scene ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \message model -> ( update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }
