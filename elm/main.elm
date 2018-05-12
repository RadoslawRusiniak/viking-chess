module Main exposing (main)

import Messages exposing (Msg(..))
import Model exposing (Model)
import Navigation
import Subscriptions
import Update
import View


main : Program Never Model Msg
main =
    Navigation.program
        --TODO how to ignore
        (always Dummy)
        { init = Update.init
        , view = View.view Clicked GetHint GetScore Prev Next ChangeSide EditPosition FinishEditing
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
