module PythagoreanTiling exposing (main)

import Angle exposing (Angle)
import Browser
import Color
import Direction2d
import Element
import FlatColors.IndianPalette as Palette
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Pixels exposing (Pixels, inPixels, pixels)
import Point2d
import Quantity exposing (Quantity)
import Rectangle2d
import Svg exposing (Svg)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (CoordinateSystem(..), Paint(..))
import Utils.Color
import Utils.Slider
import Vector2d exposing (Vector2d)



---- MODEL ----


type alias Model =
    { lengthA : Quantity Float Pixels
    , lengthB : Quantity Float Pixels
    }


init : Model
init =
    { lengthA = pixels 40.0
    , lengthB = pixels 14.0
    }



---- UPDATE ----


type Msg
    = LengthGreen Float
    | LengthPurple Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        LengthGreen input ->
            { model | lengthA = pixels input }

        LengthPurple input ->
            { model | lengthB = pixels input }



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.spacing 30
            , Element.padding 30
            , Element.centerX
            ]
            [ Utils.Slider.slider
                { label = "Side length of green square:"
                , value = inPixels model.lengthA
                , msg = LengthGreen
                , min = 6
                , max = 60
                }
            , Utils.Slider.slider
                { label = "Side length of purple square:"
                , value = inPixels model.lengthB
                , msg = LengthPurple
                , min = 6
                , max = 60
                }
            , Element.el [] <| Element.html <| myPattern model
            ]



---- Colors ----


green : Color.Color
green =
    Utils.Color.convert Palette.oasisStreamRgb


purple : Color.Color
purple =
    Utils.Color.convert Palette.brightUbeRgb



---- Pythagorean tiling ----


type YUpCoordinates
    = YUpCoordinates


type YDownCoordinates
    = YDownCoordinates


type TopLeftCorner
    = TopLeftCorner


myPattern : Model -> Html Msg
myPattern model =
    let
        sceneWidth : Quantity number Pixels
        sceneWidth =
            pixels 500

        mySquare : Quantity Float Pixels -> Color.Color -> Svg msg
        mySquare side color =
            Svg.rectangle2d
                [ TypedSvg.Attributes.fill <| Paint color
                , TypedSvg.Attributes.stroke <| Paint Color.black
                , TypedSvg.Attributes.InPx.strokeWidth 1
                ]
            <|
                Rectangle2d.from Point2d.origin (Point2d.xy side side)

        -- The names of these -ish-vectors only makes sense when
        -- lengthGreen is significantly longer than lengthPurple
        -- (like green > 3 * purple).
        rightish : Vector2d Pixels YUpCoordinates
        rightish =
            Vector2d.xy model.lengthA model.lengthB

        leftish : Vector2d Pixels YUpCoordinates
        leftish =
            rightish |> Vector2d.reverse

        upish : Vector2d Pixels YUpCoordinates
        upish =
            Vector2d.perpendicularTo rightish

        greenSquare : Svg msg
        greenSquare =
            mySquare model.lengthA green

        purpleSquare : Svg msg
        purpleSquare =
            mySquare model.lengthB purple
                |> Svg.translateBy downB

        downB : Vector2d Pixels YUpCoordinates
        downB =
            Vector2d.withLength model.lengthB Direction2d.negativeY

        -- This is all the geometry needed to fill up our svg "pattern window."
        visibleSquares : Svg msg
        visibleSquares =
            TypedSvg.g []
                [ greenSquare
                , greenSquare
                    |> Svg.translateBy leftish
                , greenSquare
                    |> Svg.translateBy upish
                , purpleSquare
                    |> Svg.translateBy upish
                , purpleSquare
                    |> Svg.translateBy upish
                    |> Svg.translateBy rightish
                , purpleSquare
                    |> Svg.translateBy (Vector2d.twice upish)
                    |> Svg.translateBy rightish
                ]

        -- For clockwise rotation of our visibleSquares so they can
        -- be seamlessly tiled by svg-pattern (we want visibleSquares
        -- to line up with the "pattern window" as in the left red
        -- square in the picture at
        -- https://en.wikipedia.org/wiki/Pythagorean_tiling#Pythagorean_theorem_and_dissections).
        negativeAngle : Angle
        negativeAngle =
            Angle.atan2
                (Quantity.negate model.lengthB)
                model.lengthA

        -- Side length of the "pattern window."
        hypotenuse : Quantity Float Pixels
        hypotenuse =
            Vector2d.length rightish

        patternDefinition : Svg msg
        patternDefinition =
            TypedSvg.defs []
                [ TypedSvg.pattern
                    [ TypedSvg.Attributes.id "Pattern"
                    , TypedSvg.Attributes.InPx.x 0
                    , TypedSvg.Attributes.InPx.y 0
                    , TypedSvg.Attributes.InPx.width <| inPixels hypotenuse
                    , TypedSvg.Attributes.InPx.height <| inPixels hypotenuse
                    , TypedSvg.Attributes.patternUnits
                        CoordinateSystemUserSpaceOnUse
                    ]
                    [ visibleSquares
                        |> Svg.rotateAround Point2d.origin negativeAngle
                    ]
                ]

        patternedArea : Svg msg
        patternedArea =
            Svg.rectangle2d
                [ TypedSvg.Attributes.fill <| Reference "Pattern" ]
            <|
                Rectangle2d.from Point2d.origin
                    (Point2d.xy sceneWidth sceneWidth)

        elements : Svg msg
        elements =
            TypedSvg.g []
                [ patternDefinition
                , patternedArea
                ]

        topLeftFrame : Frame2d Pixels YDownCoordinates { defines : TopLeftCorner }
        topLeftFrame =
            Frame2d.atPoint (Point2d.xy (pixels 0) sceneWidth)
                |> Frame2d.reverseY

        scene : Svg msg
        scene =
            elements |> Svg.relativeTo topLeftFrame
    in
    Svg.svg
        [ TypedSvg.Attributes.InPx.width (inPixels sceneWidth)
        , TypedSvg.Attributes.InPx.height (inPixels sceneWidth)
        ]
        [ scene ]



---- Main ----


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
