module Main exposing (main)

import Maybe exposing (withDefault)
import Http
import Navigation
import UrlParser exposing ((<?>), stringParam)
import Model exposing (Msg(..), Model)
import Subscriptions
import Update
import View


main : Program Never Model Msg
main =
    Navigation.program
        --TODO how to ignore
        (always Dummy)
        { init = init
        , view = View.view Clicked GetHint GetScore Prev Next
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        --TODO how to get rid of "main.elm"
        parser : UrlParser.Parser (Maybe String -> a) a
        parser =
            UrlParser.s "main.elm" <?> stringParam "password"

        parsePassword =
            UrlParser.parsePath parser >> withDefault Nothing >> withDefault "wrong"
    in
        ( Model.emptyModel
        , initGame <| parsePassword location
        )


initGame : String -> Cmd Msg
initGame pswrd =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "password" pswrd ]
                , url = Update.server ++ "initGame"
                , body = Http.emptyBody
                , expect = Http.expectJson Model.initGameDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send InitGameResponse request
