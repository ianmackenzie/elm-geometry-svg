module Utils.Slider exposing (Input, slider)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


type alias Input msg =
    { label : String
    , value : Float
    , msg : Float -> msg
    , min : Float
    , max : Float
    }


grey : Element.Color
grey =
    Element.rgb 0.5 0.5 0.5


slider : Input msg -> Element msg
slider input =
    let
        track =
            el
                [ width fill
                , height <| px 2
                , centerY
                , Background.color grey
                ]
                none

        label =
            Input.labelAbove
                [ Font.color grey ]
                (text input.label)
    in
    Input.slider
        [ behindContent track ]
        { onChange = input.msg
        , label = label
        , min = input.min
        , max = input.max
        , value = input.value
        , thumb = Input.defaultThumb
        , step = Nothing
        }
