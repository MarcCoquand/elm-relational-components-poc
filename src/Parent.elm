module Parent exposing (Model, Msg(..), init, makeMessage, update, view)

import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { count : Int
    }


type Msg
    = Up
    | Down


update : Msg -> Model -> Model
update cmd mdl =
    case cmd of
        Up ->
            { mdl | count = mdl.count + 1 }

        Down ->
            { mdl | count = mdl.count - 1 }


makeMessage : (Msg -> msg) -> Model -> Msg -> Cmd msg
makeMessage merger mdl msg =
    case msg of
        Up ->
            Cmd.none

        Down ->
            Cmd.none


view : (Msg -> msg) -> Model -> Html msg
view sendCmd mdl =
    div []
        [ button [ onClick (sendCmd Up) ] [ Html.text "up" ]
        , Html.text
            (String.fromInt
                mdl.count
            )
        , button [ onClick (sendCmd Down) ] [ Html.text "down" ]
        ]


init : Model
init =
    { count = 5 }
