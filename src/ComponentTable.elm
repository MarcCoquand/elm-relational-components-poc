module ComponentTable exposing (Model, Msg, init, update, view)

import Child
import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Parent
import Set exposing (Set)


type Component
    = Count
    | CountParent


type alias ComponentId =
    Int


type Msg
    = GotCountMsg ComponentId Child.Model Child.Msg
    | GotParentMsg ComponentId Parent.Model Parent.Msg
    | SetView Component ComponentId


type alias Model =
    { renderId : ( Component, ComponentId )
    , parents : Dict ComponentId Parent.Model
    , children : Dict ComponentId Child.Model
    }


updateParent : ComponentId -> Model -> Parent.Model -> Model
updateParent id mdl newMdl =
    { mdl | parents = Dict.insert id newMdl mdl.parents }


updateChildren : ComponentId -> Model -> Child.Model -> Model
updateChildren id mdl newMdl =
    { mdl | children = Dict.insert id newMdl mdl.children }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        SetView component newId ->
            ( { mdl | renderId = ( component, newId ) }, Cmd.none )

        GotParentMsg id newComponent innerMsg ->
            ( Parent.update innerMsg newComponent
                |> updateParent id mdl
            , Parent.makeMessage
                (GotParentMsg id newComponent)
                newComponent
                innerMsg
            )

        GotCountMsg id newComponent innerMsg ->
            ( Child.update innerMsg newComponent
                |> updateChildren id mdl
            , Child.makeMessage (GotCountMsg id newComponent)
                newComponent
                innerMsg
            )


makeChildView : ComponentId -> Child.Model -> Html Msg
makeChildView renderId mdl =
    Child.view (GotCountMsg renderId mdl) mdl


makeParentView :
    Dict ComponentId Child.Model
    -> ComponentId
    -> Parent.Model
    -> Html Msg
makeParentView components parentId parentModel =
    queryChildren parentId components
        |> Child.nestedView (GotParentMsg parentId parentModel) parentModel


queryChildren :
    ComponentId
    -> Dict ComponentId Child.Model
    -> List ( Child.Msg -> Msg, Child.Model )
queryChildren parentId components =
    Dict.foldl
        (\id component list ->
            if component.parentId == Just parentId then
                list ++ [ ( GotCountMsg id component, component ) ]

            else
                list
        )
        []
        components


render : Model -> Maybe (Html Msg)
render state =
    let
        ( component, id ) =
            state.renderId
    in
    case component of
        Count ->
            Dict.get id state.children
                |> Maybe.map (makeChildView id)

        CountParent ->
            Dict.get id state.parents
                |> Maybe.map (makeParentView state.children id)


view : Model -> Html Msg
view state =
    let
        maybeRender =
            render state

        ( _, currentId ) =
            state.renderId
    in
    case maybeRender of
        Just component ->
            div []
                [ button [ onClick (SetView Count 1) ] [ Html.text "Click for view 1" ]
                , button [ onClick (SetView CountParent 3) ] [ Html.text "Click for view 3" ]
                , button [ onClick (SetView Count 4) ] [ Html.text "Click for view 4" ]
                , component
                ]

        Nothing ->
            Html.text ("Unknown id: " ++ String.fromInt currentId)


initialChildren : Dict ComponentId Child.Model
initialChildren =
    Dict.fromList
        [ ( 1, Child.init1 Nothing )
        , ( 4, Child.init1 (Just 3) )
        , ( 5, Child.init1 (Just 3) )
        , ( 6, Child.init1 (Just 3) )
        ]


initialParents : Dict ComponentId Parent.Model
initialParents =
    Dict.fromList
        [ ( 3, Parent.init )
        ]


init : Model
init =
    { renderId = ( Count, 4 )
    , children = initialChildren
    , parents = initialParents
    }
