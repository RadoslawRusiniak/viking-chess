module Main exposing (main)

import Model exposing (Msg(..), Model)
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
        , view = View.view Clicked GetHint GetScore Prev Next
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
