module Child exposing (Model, Msg(..), init, nestedView, update, view)

import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)
import Parent



-- MODEL


type alias Model msg =
    { count : Int, sendMsg : Msg -> msg, parentId : Maybe Int }


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
view model =
    div []
        [ button [ onClick (model.sendMsg Up) ] [ Html.text "up" ]
        , Html.text
            (String.fromInt
                model.count
            )
        , button [ onClick (model.sendMsg Down) ] [ Html.text "down" ]
        ]


nestedView :
    Parent.Model msg
    -> List (Model msg)
    -> Html msg
nestedView parentMdl children =
    div []
        [ Parent.view parentMdl
        , div []
            (List.map
                (\childMdl -> view childMdl)
                children
            )
        ]


init : { childMsg : Msg -> msg, parentId : Maybe Int } -> Model msg
init mdl =
    { count = 5, sendMsg = mdl.childMsg, parentId = mdl.parentId }
