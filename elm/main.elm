module Main exposing (main)

import Messages exposing (Msg(..))
import Model exposing (Model)
--import Navigation
import Browser
import Subscriptions
import Update
import View


main : Program () Model Msg
main =
    Browser.application
        { init = (\_ addr _ -> Update.init addr)
        , view = View.view Clicked GetHint GetScore Prev Next ChangeSide EditPosition ClearPawns FinishEditing
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }