module Interpolation exposing (..)

import Circle2d
import CubicSpline2d
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Kintail.InputWidget as InputWidget
import Point2d
import Svg
import Svg.Attributes


type alias Model =
    { x0 : Float
    }


init : Model
init =
    { x0 = 0.5 }


type Msg
    = NewValue Float


update : Msg -> Model -> Model
update (NewValue newValue) _ =
    { x0 = newValue }


view : Model -> Html Msg
view { x0 } =
    let
        topLeftFrame =
            Frame2d.atCoordinates ( 0, 400 ) |> Frame2d.reverseY

        width =
            300

        height =
            500

        y1 =
            (2 - x0) / (3 * (1 - x0))

        y2 =
            (2 * x0 - 1) / (3 * x0)

        spline =
            CubicSpline2d.with
                { startPoint = Point2d.origin
                , startControlPoint = Point2d.fromCoordinates ( 1 / 3, y1 )
                , endControlPoint = Point2d.fromCoordinates ( 2 / 3, y2 )
                , endPoint = Point2d.fromCoordinates ( 1, 1 )
                }

        svg =
            Svg.g []
                [ Svg.cubicSpline2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "0.005"
                    ]
                    spline
                , Svg.circle2d [] <|
                    Circle2d.withRadius 0.02
                        (Point2d.fromCoordinates ( x0, x0 ))
                ]
    in
    Html.div []
        [ Html.div []
            [ Svg.svg
                [ Svg.Attributes.width (toString width)
                , Svg.Attributes.height (toString height)
                , Html.Attributes.style [ ( "display", "block" ) ]
                ]
                [ svg
                    |> Svg.scaleAbout Point2d.origin 300
                    |> Svg.relativeTo topLeftFrame
                ]
            ]
        , InputWidget.slider [ Html.Attributes.style [ ( "width", "300px" ) ] ]
            { min = 0.01, max = 0.99, step = 0.01 }
            x0
            |> Html.map NewValue
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
