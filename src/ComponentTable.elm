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


makeParentView : ComponentId -> Parent.Model -> Html Msg
makeParentView renderId mdl =
    Parent.view (GotParentMsg renderId mdl) mdl


wireChildParent :
    Child.Model
    -> ComponentId
    -> ComponentId
    -> Parent.Model
    -> Html Msg
wireChildParent childModel childId parentId parentModel =
    Child.nestedView
        (GotCountMsg childId childModel)
        childModel
        (GotParentMsg parentId parentModel)
        parentModel


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


makeChild :
    Dict ComponentId Component
    -> ComponentId
    -> Child.Model
    -> Maybe (Html Msg)
makeChild components id model =
    case model.parentId of
        Just parentId ->
            parentId
                |> queryParent components
                |> Maybe.map (wireChildParent model id parentId)

        Nothing ->
            Just (makeChildView id model)


construct :
    Dict ComponentId Component
    -> ComponentId
    -> Component
    -> Maybe (Html Msg)
construct components id component =
    case component of
        Count model ->
            makeChild components id model

        CountParent model ->
            Just (makeParentView id model)


render : ComponentId -> Dict ComponentId Component -> Maybe (Html Msg)
render id components =
    Dict.get id components
        |> Maybe.andThen (construct components id)


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
                , button [ onClick (SetView 4) ] [ Html.text "Click for view 3" ]
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
    { renderId = 1
    , components = initialComponents
    }
