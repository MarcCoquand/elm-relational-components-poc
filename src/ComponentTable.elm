module ComponentTable exposing (Component(..), ComponentId, Model, Msg(..), init, initialComponents, makeParentView, makeSimpleView, renderComponent, update, updatePage, view)

import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Parent
import Set exposing (Set)
import Simple


type Component
    = Count Simple.Model
    | CountParent Parent.Model


type alias ComponentId =
    Int


type Msg
    = GotCountMsg ComponentId Simple.Model Simple.Msg
    | GotParentMsg ComponentId Parent.Model Parent.Msg
    | SetView ComponentId


type alias Model =
    { renderId : ComponentId
    , components : Dict ComponentId Component
    }


updatePage : ComponentId -> Component -> Model -> Model
updatePage id newMdl mdl =
    let
        maybePage =
            Dict.get id mdl.components
    in
    case maybePage of
        Just page ->
            { mdl | components = Dict.insert id newMdl mdl.components }

        Nothing ->
            mdl


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        SetView newId ->
            ( { mdl | renderId = newId }, Cmd.none )

        GotParentMsg id newComponent nextMsg ->
            ( updatePage id (CountParent newComponent) mdl
            , Cmd.map
                (GotParentMsg id newComponent)
                (Parent.makeMessage newComponent nextMsg)
            )

        GotCountMsg id newComponent nextMsg ->
            ( updatePage id (Count newComponent) mdl
            , Cmd.map
                (GotCountMsg id newComponent)
                (Simple.makeMessage newComponent nextMsg)
            )


makeSimpleView : ComponentId -> Simple.Model -> Html Msg
makeSimpleView renderId mdl =
    Html.map
        (\msg -> GotCountMsg renderId (Simple.update msg mdl) msg)
        (Simple.view mdl)


makeParentView : ComponentId -> Parent.Model -> ComponentId -> Simple.Model -> Html Msg
makeParentView renderId parent childId child =
    Html.map
        (\incMsg ->
            case incMsg of
                Parent.Parent msg ->
                    GotParentMsg renderId (Parent.update msg parent) msg

                Parent.Child msg ->
                    GotCountMsg childId
                        (Simple.update msg child)
                        msg
        )
        (Parent.view (Simple.view child) parent)


getChild : ComponentId -> Dict ComponentId Component -> Maybe Simple.Model
getChild id components =
    Dict.get id components
        |> Maybe.andThen
            (\component ->
                case component of
                    Count model ->
                        Just model

                    _ ->
                        Nothing
            )


renderComponent : ComponentId -> Dict ComponentId Component -> Maybe (Html Msg)
renderComponent id components =
    Dict.get id components
        |> Maybe.andThen
            (\component ->
                case component of
                    Count model ->
                        Just (makeSimpleView id model)

                    CountParent model ->
                        getChild model.childId components
                            |> Maybe.map (makeParentView id model model.childId)
            )


view : Model -> Html Msg
view state =
    let
        maybeRender =
            renderComponent state.renderId state.components
    in
    case maybeRender of
        Just component ->
            div []
                [ button [ onClick (SetView 1) ] [ Html.text "Click for view 1" ]
                , button [ onClick (SetView 3) ] [ Html.text "Click for view 3" ]
                , component
                ]

        Nothing ->
            Html.text "Nothing to render"


initialComponents : Dict ComponentId Component
initialComponents =
    Dict.fromList
        [ ( 1, Count Simple.init1 )
        , ( 2, Count Simple.init2 )
        , ( 4, Count Simple.init2 )
        , ( 3, CountParent (Parent.init 4) )
        ]


init : Model
init =
    { renderId = 4
    , components = initialComponents
    }
