module Logo exposing (Model, Msg(..), init, inputField, logo, main, update, view)

import Angle exposing (Angle)
import Browser
import Frame2d
import Frame3d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import Point2d
import Point3d
import Polygon2d
import Quantity
import Svg exposing (Svg)
import Svg.Attributes


type alias Model =
    { heightInput : String
    , xOffsetInput : String
    , yOffsetInput : String
    , zOffsetInput : String
    , azimuthInput : String
    , elevationInput : String
    , height : Float
    , xOffset : Float
    , yOffset : Float
    , zOffset : Float
    , azimuth : Angle
    , elevation : Angle
    }


init : Model
init =
    { heightInput = "0.8"
    , xOffsetInput = "0.6"
    , yOffsetInput = "0.6"
    , zOffsetInput = "0.6"
    , azimuthInput = "65"
    , elevationInput = "20"
    , height = 0.8
    , xOffset = 0.6
    , yOffset = 0.6
    , zOffset = 0.6
    , azimuth = Angle.degrees 65
    , elevation = Angle.degrees 20
    }


type Msg
    = HeightInput String
    | XOffsetInput String
    | YOffsetInput String
    | ZOffsetInput String
    | AzimuthInput String
    | ElevationInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        HeightInput input ->
            { model
                | heightInput = input
                , height = String.toFloat input |> Maybe.withDefault model.height
            }

        XOffsetInput input ->
            { model
                | xOffsetInput = input
                , xOffset = String.toFloat input |> Maybe.withDefault model.xOffset
            }

        YOffsetInput input ->
            { model
                | yOffsetInput = input
                , yOffset = String.toFloat input |> Maybe.withDefault model.yOffset
            }

        ZOffsetInput input ->
            { model
                | zOffsetInput = input
                , zOffset = String.toFloat input |> Maybe.withDefault model.zOffset
            }

        AzimuthInput input ->
            { model
                | azimuthInput = input
                , azimuth =
                    String.toFloat input |> Maybe.map Angle.degrees |> Maybe.withDefault model.azimuth
            }

        ElevationInput input ->
            { model
                | elevationInput = input
                , elevation =
                    String.toFloat input |> Maybe.map Angle.degrees |> Maybe.withDefault model.elevation
            }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.form []
            [ inputField "Height:" model.heightInput HeightInput
            , inputField "X offset:" model.xOffsetInput XOffsetInput
            , inputField "Y offset:" model.yOffsetInput YOffsetInput
            , inputField "Z offset:" model.zOffsetInput ZOffsetInput
            , inputField "Azimuth:" model.azimuthInput AzimuthInput
            , inputField "Elevation:" model.elevationInput ElevationInput
            ]
        , logo model
        ]


inputField : String -> String -> (String -> Msg) -> Html Msg
inputField label value msg =
    Html.div []
        [ Html.label [] [ Html.text label ]
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value value
            , Events.onInput msg
            ]
            []
        ]


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
            "rgb(240, 173, 0)"

        green =
            "rgb(127, 209, 59)"

        lightBlue =
            "rgb(96, 181, 204)"

        darkBlue =
            "rgb(90, 99, 120)"

        mask id polygon =
            let
                attributes =
                    [ Svg.Attributes.fill "white"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "0.03"
                    ]
            in
            Svg.mask [ Svg.Attributes.id id ]
                [ Svg.polygon2d attributes polygon ]

        face color clipPathId polygon =
            let
                attributes =
                    [ Svg.Attributes.fill color
                    , Svg.Attributes.mask ("url(#" ++ clipPathId ++ ")")
                    ]
            in
            Svg.polygon2d attributes polygon

        defs =
            Svg.defs []
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
            Svg.g [] [ defs, leftFace, rightFace, topFace, triangleFace ]

        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels -250 250)
                |> Frame2d.reverseY

        scene =
            Svg.relativeTo topLeftFrame
                (Svg.scaleAbout Point2d.origin 200 elements)
    in
    Svg.svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ]
        [ scene ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \message model -> ( update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }
