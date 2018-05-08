module Update exposing (init, update)

import Http
import Maybe exposing (withDefault)
import Matrix
import Model exposing (Model, Msg(..), GameState)
import Navigation
import UrlParser exposing ((<?>), stringParam)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        handleErr e =
            ( { model | errorText = toString e }, Cmd.none )

        getHead =
            List.head >> withDefault Model.emptyState

        getTail =
            List.tail >> withDefault []

        zipNext m =
            { m
                | historyPrev = m.state :: m.historyPrev
                , state = getHead m.historyNext
                , historyNext = getTail m.historyNext
            }

        zipPrev m =
            { m
                | historyPrev = getTail m.historyPrev
                , state = getHead m.historyPrev
                , historyNext = m.state :: m.historyNext
            }

        handleClick : Model -> Matrix.Location -> ( Model, Cmd Msg )
        handleClick model location =
            case model.clickedLocation of
                Nothing ->
                    ( { model | clickedLocation = Just location }
                    , getReachablePositions model.token model.state location
                    )

                Just from ->
                    case List.member location (model.possibleMoves |> withDefault []) of
                        True ->
                            ( { model | clickedLocation = Nothing }, makeMove model.token model.state from location )

                        False ->
                            ( { model | clickedLocation = Nothing, possibleMoves = Nothing }, Cmd.none )
    in
        case msg of
            Dummy ->
                ( model, Cmd.none )

            InitGameResponse (Err e) ->
                handleErr e

            InitGameResponse (Ok ( t, s )) ->
                ( { model | token = t, state = s }, Cmd.none )

            Clicked location ->
                handleClick model location

            Next ->
                ( zipNext model, Cmd.none )

            Prev ->
                ( zipPrev model, Cmd.none )

            GetScore ->
                ( model, getCurrentScore model.token model.state )

            GetScoreResponse (Err e) ->
                handleErr e

            GetScoreResponse (Ok x) ->
                ( { model | currentScore = x }, Cmd.none )

            GetMovesResponse (Err e) ->
                handleErr e

            GetMovesResponse (Ok locs) ->
                ( { model | possibleMoves = Just locs, hint = Nothing }
                , Cmd.none
                )

            MakeMoveResponse (Err e) ->
                handleErr e

            MakeMoveResponse (Ok s) ->
                ( { model
                    | errorText = ""
                    , state = s
                    , possibleMoves = Nothing
                    , hint = Nothing
                    , historyPrev = model.state :: model.historyPrev
                    , historyNext = []
                  }
                , Cmd.none
                )

            GetHint ->
                ( model, getHint model.token model.state )

            GetHintResponse (Err e) ->
                handleErr e

            GetHintResponse (Ok mv) ->
                ( { model | clickedLocation = Nothing, possibleMoves = Nothing, hint = Just mv }
                , Cmd.none
                )


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


server : String
server =
    "http://localhost:5000/"


initGame : String -> Cmd Msg
initGame pswrd =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "password" pswrd ]
                , url = server ++ "initGame"
                , body = Http.emptyBody
                , expect = Http.expectJson Model.initGameDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send InitGameResponse request


getHint : String -> GameState -> Cmd Msg
getHint token state =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url =
                    server
                        ++ "getHint"
                        ++ "?state="
                        ++ Model.stateEncoder state
                , body = Http.emptyBody
                , expect = Http.expectJson Model.hintDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send GetHintResponse request


makeMove : String -> GameState -> Matrix.Location -> Matrix.Location -> Cmd Msg
makeMove token state locFrom locTo =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url =
                    server
                        ++ "makeMove"
                        ++ "?state="
                        ++ Model.stateEncoder state
                        ++ "&from="
                        ++ Model.locationEncoder locFrom
                        ++ "&to="
                        ++ Model.locationEncoder locTo
                , body = Http.emptyBody
                , expect = Http.expectJson Model.gameStateDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send MakeMoveResponse request


getReachablePositions : String -> GameState -> Matrix.Location -> Cmd Msg
getReachablePositions token state location =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url =
                    server
                        ++ "getReachablePositions"
                        ++ "?state="
                        ++ Model.stateEncoder state
                        ++ "&location="
                        ++ Model.locationEncoder location
                , body = Http.emptyBody
                , expect = Http.expectJson Model.locationListDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send GetMovesResponse request


getCurrentScore : String -> GameState -> Cmd Msg
getCurrentScore token state =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url =
                    server
                        ++ "getScore"
                        ++ "?state="
                        ++ Model.stateEncoder state
                , body = Http.emptyBody
                , expect = Http.expectJson Model.scoreDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send GetScoreResponse request
