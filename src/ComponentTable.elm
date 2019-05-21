module ComponentTable exposing (Model, Msg, init, update, view)

import Child
import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Parent
import Set exposing (Set)


type ComponentName
    = Count
    | CountParent


type alias ComponentId =
    Int


type Msg
    = GotCountMsg ComponentId Child.Msg
    | GotParentMsg ComponentId Parent.Msg
    | SetView ComponentName ComponentId


type alias Component a =
    Dict ComponentId a


type alias Model =
    { renderId : ( ComponentName, ComponentId )
    , parents : Component (Parent.Model Msg)
    , children : Component (Child.Model Msg)
    }


type alias ComponentUpdate model =
    ( Component model, Cmd Msg )


updateComponent :
    { update : msg -> model -> ( model, Cmd Msg )
    , msg : msg
    , component : Component model
    , id : ComponentId
    , setRecord : ComponentUpdate model -> ( Model, Cmd Msg )
    }
    -> ( Model, Cmd Msg )
updateComponent arg =
    Dict.get arg.id arg.component
        |> Maybe.map (arg.update arg.msg)
        |> Maybe.map (\( mdl, cmd ) -> ( Dict.insert arg.id mdl arg.component, cmd ))
        |> Maybe.withDefault ( arg.component, Cmd.none )
        |> arg.setRecord


parentSetter : Model -> ComponentUpdate (Parent.Model Msg) -> ( Model, Cmd Msg )
parentSetter mdl ( component, msg ) =
    ( { mdl | parents = component }, msg )


childSetter : Model -> ComponentUpdate (Child.Model Msg) -> ( Model, Cmd Msg )
childSetter mdl ( component, msg ) =
    ( { mdl | children = component }, msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        SetView component newId ->
            ( { mdl | renderId = ( component, newId ) }, Cmd.none )

        GotParentMsg id innerMsg ->
            updateComponent
                { update = Parent.update
                , msg = innerMsg
                , component = mdl.parents
                , id = id
                , setRecord = parentSetter mdl
                }

        GotCountMsg id innerMsg ->
            updateComponent
                { update = Child.update
                , msg = innerMsg
                , component = mdl.children
                , id = id
                , setRecord = childSetter mdl
                }


makeParentView :
    Dict ComponentId (Child.Model Msg)
    -> ComponentId
    -> Parent.Model Msg
    -> Html Msg
makeParentView components parentId parentModel =
    queryChildren parentId components
        |> Child.nestedView parentModel


queryChildren :
    ComponentId
    -> Dict ComponentId (Child.Model Msg)
    -> List (Child.Model Msg)
queryChildren parentId components =
    Dict.foldl
        (\id component list ->
            if component.parentId == Just parentId then
                list ++ [ component ]

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
                |> Maybe.map Child.view

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


initialChildren : Dict ComponentId (Child.Model Msg)
initialChildren =
    let
        entry id parentId =
            ( id, Child.init { childMsg = GotCountMsg id, parentId = parentId } )
    in
    Dict.fromList
        [ entry 1 Nothing
        , entry 4 (Just 3)
        , entry 5 (Just 3)
        , entry 6 (Just 3)
        ]


initialParents : Dict ComponentId (Parent.Model Msg)
initialParents =
    Dict.fromList
        [ ( 3, Parent.init (GotParentMsg 3) )
        ]


init : Model
init =
    { renderId = ( Count, 4 )
    , children = initialChildren
    , parents = initialParents
    }
