module Main exposing (main)

import Browser
import ComponentTable
import Html exposing (Html)


main : Program () ComponentTable.Model ComponentTable.Msg
main =
    Browser.element
        { init = \_ -> ( ComponentTable.init, Cmd.none )
        , view = ComponentTable.view
        , update = ComponentTable.update
        , subscriptions = \_ -> Sub.none
        }
