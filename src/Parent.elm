module Parent exposing (From(..), Model, Msg(..), init, makeMessage, update, view)

import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)
import Simple



-- MODEL


type alias Model =
    { count : Int
    , childId : Int
    }


type Msg
    = Up
    | Down


type From
    = Parent Msg
    | Child Simple.Msg


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


view : Html Simple.Msg -> Model -> Html From
view simpleView model =
    div []
        [ button [ onClick (Parent Up) ] [ Html.text "up" ]
        , Html.text
            (String.fromInt
                model.count
            )
        , button [ onClick (Parent Down) ] [ Html.text "down" ]
        , Html.map Child simpleView
        ]


init : Int -> Model
init i =
    { count = 5, childId = i }
