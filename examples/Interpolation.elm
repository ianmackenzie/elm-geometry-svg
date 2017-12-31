module Interpolation exposing (..)

import Html exposing (Html)
import Html.Attributes
import Kintail.InputWidget as InputWidget
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Svg as Svg
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
        boundingBox =
            BoundingBox2d.with
                { minX = 0
                , maxX = 300
                , minY = -100
                , maxY = 400
                }

        y1 =
            (2 - x0) / (3 * (1 - x0))

        y2 =
            (2 * x0 - 1) / (3 * x0)

        spline =
            CubicSpline2d.fromControlPoints
                ( Point2d.origin
                , Point2d.fromCoordinates ( 1 / 3, y1 )
                , Point2d.fromCoordinates ( 2 / 3, y2 )
                , Point2d.fromCoordinates ( 1, 1 )
                )

        svg =
            Svg.g []
                [ Svg.cubicSpline2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "0.005"
                    ]
                    spline
                , Svg.point2dWith { radius = 0.02 } [] <|
                    Point2d.fromCoordinates ( x0, x0 )
                ]
    in
    Html.div []
        [ Html.div []
            [ Svg.render2d boundingBox (Svg.scaleAbout Point2d.origin 300 svg) ]
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
