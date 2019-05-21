module Parent exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)



-- MODEL


type alias Model msg =
    { count : Int
    , sendMsg : Msg -> msg
    }


type Msg
    = Up
    | Down


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update cmd mdl =
    case cmd of
        Up ->
            ( { mdl | count = mdl.count + 1 }, Cmd.none )

        Down ->
            ( { mdl | count = mdl.count - 1 }, Cmd.none )


view : Model msg -> Html msg
view mdl =
    div []
        [ button [ onClick (mdl.sendMsg Up) ] [ Html.text "up" ]
        , Html.text
            (String.fromInt
                mdl.count
            )
        , button [ onClick (mdl.sendMsg Down) ] [ Html.text "down" ]
        ]


init : (Msg -> msg) -> Model msg
init msgMaker =
    { count = 5, sendMsg = msgMaker }
