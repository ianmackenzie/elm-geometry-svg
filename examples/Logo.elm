module Logo exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Polygon2d as Polygon2d
import OpenSolid.Svg as Svg


type alias Model =
    { height : Float
    , xOffset : Float
    , yOffset : Float
    , zOffset : Float
    , azimuth : Float
    , elevation : Float
    }


init : Model
init =
    { height = 0.8
    , xOffset = 0.6
    , yOffset = 0.6
    , zOffset = 0.6
    , azimuth = degrees 65
    , elevation = degrees 20
    }


type Msg
    = Height String
    | XOffset String
    | YOffset String
    | ZOffset String
    | Azimuth String
    | Elevation String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Height input ->
            let
                newValue =
                    Result.withDefault model.height (String.toFloat input)
            in
                { model | height = newValue }

        XOffset input ->
            let
                newValue =
                    Result.withDefault model.xOffset (String.toFloat input)
            in
                { model | xOffset = newValue }

        YOffset input ->
            let
                newValue =
                    Result.withDefault model.yOffset (String.toFloat input)
            in
                { model | yOffset = newValue }

        ZOffset input ->
            let
                newValue =
                    Result.withDefault model.zOffset (String.toFloat input)
            in
                { model | zOffset = newValue }

        Azimuth input ->
            let
                newValue =
                    Result.withDefault model.azimuth
                        (Result.map degrees (String.toFloat input))
            in
                { model | azimuth = newValue }

        Elevation input ->
            let
                newValue =
                    Result.withDefault model.elevation
                        (Result.map degrees (String.toFloat input))
            in
                { model | elevation = newValue }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.form []
            [ inputField "Height:" init.height Height
            , inputField "X offset:" init.xOffset XOffset
            , inputField "Y offset:" init.yOffset YOffset
            , inputField "Z offset:" init.zOffset ZOffset
            , inputField "Azimuth:" (init.azimuth / degrees 1) Azimuth
            , inputField "Elevation:" (init.elevation / degrees 1) Elevation
            ]
        , logo model
        ]


inputField : String -> Float -> (String -> Msg) -> Html Msg
inputField label defaultValue msg =
    Html.div []
        [ Html.label [] [ Html.text label ]
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.defaultValue (toString defaultValue)
            , Events.onInput msg
            ]
            []
        ]


logo : Model -> Html Msg
logo model =
    let
        p1 =
            Point3d ( 1, 0, 0 )

        p2 =
            Point3d ( 1, 1, 0 )

        p3 =
            Point3d ( 0, 1, 0 )

        p4 =
            Point3d ( 0, 1, model.height )

        p5 =
            Point3d ( 0, 0, model.height )

        p6 =
            Point3d ( 1, 0, model.height )

        p7 =
            Point3d ( 1, 1 - model.yOffset, model.height )

        p8 =
            Point3d ( 1, 1, model.height - model.zOffset )

        p9 =
            Point3d ( 1 - model.xOffset, 1, model.height )

        viewFrame =
            Frame3d.at (Point3d ( 0.5, 0.5, model.height / 2 ))
                |> Frame3d.rotateAroundOwn Frame3d.zAxis model.azimuth
                |> Frame3d.rotateAroundOwn Frame3d.yAxis (-model.elevation)

        to2d =
            Point3d.projectInto (Frame3d.yzSketchPlane viewFrame)

        leftPolygon =
            Polygon2d (List.map to2d [ p1, p2, p8, p7, p6 ])

        rightPolygon =
            Polygon2d (List.map to2d [ p2, p3, p4, p9, p8 ])

        topPolygon =
            Polygon2d (List.map to2d [ p6, p7, p9, p4, p5 ])

        trianglePolygon =
            Polygon2d (List.map to2d [ p7, p8, p9 ])

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
            Frame2d
                { originPoint = Point2d ( -250, 250 )
                , xDirection = Direction2d.positiveX
                , yDirection = Direction2d.negativeY
                }

        scene =
            Svg.relativeTo topLeftFrame
                (Svg.scaleAbout Point2d.origin 200 elements)
    in
        Svg.svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ]
            [ scene ]


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, update = update, view = view }
