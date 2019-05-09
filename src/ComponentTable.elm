module ComponentTable exposing (Model, Msg, init, update, view)

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


updateComponent : ComponentId -> Model -> Component -> Model
updateComponent id mdl newMdl =
    { mdl | components = Dict.insert id newMdl mdl.components }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        SetView newId ->
            ( { mdl | renderId = newId }, Cmd.none )

        GotParentMsg id newComponent innerMsg ->
            ( CountParent (Parent.update innerMsg newComponent)
                |> updateComponent id mdl
            , Parent.makeMessage
                (GotParentMsg id newComponent)
                newComponent
                innerMsg
            )

        GotCountMsg id newComponent innerMsg ->
            ( Count (Child.update innerMsg newComponent)
                |> updateComponent id mdl
            , Child.makeMessage (GotCountMsg id newComponent)
                newComponent
                innerMsg
            )


makeChildView : ComponentId -> Child.Model -> Html Msg
makeChildView renderId mdl =
    Child.view (GotCountMsg renderId mdl) mdl


makeParentView :
    Dict ComponentId Component
    -> ComponentId
    -> Parent.Model
    -> Html Msg
makeParentView components parentId parentModel =
    queryChildren parentId components
        |> Child.nestedView (GotParentMsg parentId parentModel) parentModel


queryParent : Dict ComponentId Component -> ComponentId -> Maybe Parent.Model
queryParent components id =
    Dict.get id components
        |> Maybe.andThen
            (\component ->
                case component of
                    CountParent model ->
                        Just model

                    _ ->
                        Nothing
            )


queryReducer :
    ComponentId
    -> ComponentId
    -> Component
    -> List ( Child.Msg -> Msg, Child.Model )
queryReducer parentId childId component =
    case component of
        Count model ->
            if model.parentId == Just parentId then
                [ ( GotCountMsg childId model, model ) ]

            else
                []

        _ ->
            []


queryChildren :
    ComponentId
    -> Dict ComponentId Component
    -> List ( Child.Msg -> Msg, Child.Model )
queryChildren parentId components =
    Dict.foldl
        (\id component list ->
            list ++ queryReducer parentId id component
        )
        []
        components


construct :
    Dict ComponentId Component
    -> ComponentId
    -> Component
    -> Html Msg
construct components id component =
    case component of
        Count model ->
            makeChildView id model

        CountParent model ->
            makeParentView components id model


render : ComponentId -> Dict ComponentId Component -> Maybe (Html Msg)
render id components =
    Dict.get id components
        |> Maybe.map (construct components id)


view : Model -> Html Msg
view state =
    let
        maybeRender =
            render state.renderId state.components
    in
    case maybeRender of
        Just component ->
            div []
                [ button [ onClick (SetView 1) ] [ Html.text "Click for view 1" ]
                , button [ onClick (SetView 3) ] [ Html.text "Click for view 3" ]
                , button [ onClick (SetView 4) ] [ Html.text "Click for view 4" ]
                , component
                ]

        Nothing ->
            Html.text ("Unknown id: " ++ String.fromInt state.renderId)


initialComponents : Dict ComponentId Component
initialComponents =
    Dict.fromList
        [ ( 1, Count (Child.init1 Nothing) )
        , ( 4, Count (Child.init1 (Just 3)) )
        , ( 3, CountParent Parent.init )
        ]


init : Model
init =
    { renderId = 3
    , components = initialComponents
    }
