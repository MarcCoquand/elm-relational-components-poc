module ComponentTable exposing (Component(..), ComponentId, Model, Msg(..), getChild, init, initialComponents, makeChildView, makeParentView, renderComponent, update, updatePage, view)

import Child
import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Parent
import Set exposing (Set)


type Component
    = Count Child.Model
    | CountParent Parent.Model


type alias ComponentId =
    Int


type Msg
    = GotCountMsg ComponentId Child.Model Child.Msg
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
                (Child.makeMessage newComponent nextMsg)
            )


makeChildView : ComponentId -> Child.Model -> Html Msg
makeChildView renderId mdl =
    Html.map
        (\msg -> GotCountMsg renderId (Child.update msg mdl) msg)
        (Child.view mdl)


makeParentView : ComponentId -> Parent.Model -> ComponentId -> Child.Model -> Html Msg
makeParentView renderId parent childId child =
    Html.map
        (\incMsg ->
            case incMsg of
                Parent.Parent msg ->
                    GotParentMsg renderId (Parent.update msg parent) msg

                Parent.Child msg ->
                    GotCountMsg childId
                        (Child.update msg child)
                        msg
        )
        (Parent.view (Child.view child) parent)


getChild : ComponentId -> Dict ComponentId Component -> Maybe Child.Model
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
                        Just (makeChildView id model)

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
        [ ( 1, Count Child.init1 )
        , ( 2, Count Child.init2 )
        , ( 4, Count Child.init2 )
        , ( 3, CountParent (Parent.init 4) )
        ]


init : Model
init =
    { renderId = 4
    , components = initialComponents
    }
