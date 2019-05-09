module Child exposing (Model, Msg(..), init1, makeMessage, nestedView, update, view)

import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)
import Parent



-- MODEL


type alias Model =
    { count : Int, parentId : Maybe Int }


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
makeMessage merge mdl msg =
    case msg of
        Up ->
            Cmd.none

        Down ->
            Cmd.none


view : (Msg -> msg) -> Model -> Html msg
view sendMsg model =
    div []
        [ button [ onClick (sendMsg Up) ] [ Html.text "up" ]
        , Html.text
            (String.fromInt
                model.count
            )
        , button [ onClick (sendMsg Down) ] [ Html.text "down" ]
        ]


nestedView :
    (Msg -> msg)
    -> Model
    -> (Parent.Msg -> msg)
    -> Parent.Model
    -> Html msg
nestedView handleChild childMdl handleParent parentMdl =
    div []
        [ Parent.view handleParent parentMdl
        , view handleChild childMdl
        ]


init1 : Maybe Int -> Model
init1 i =
    { count = 5, parentId = i }
