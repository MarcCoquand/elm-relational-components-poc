module Child exposing (Model, Msg(..), init1, init2, makeMessage, update, view)

import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { count : Int }


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


makeMessage : Model -> Msg -> Cmd Msg
makeMessage mdl msg =
    case msg of
        Up ->
            Cmd.none

        Down ->
            Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Up ] [ Html.text "up" ]
        , Html.text
            (String.fromInt
                model.count
            )
        , button [ onClick Down ] [ Html.text "down" ]
        ]


init1 : Model
init1 =
    { count = 5 }


init2 : Model
init2 =
    { count = 3 }
