module Update exposing (init, update)

import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Maybe exposing (withDefault)
import Matrix
import Model exposing (Model, Msg(..), GameState, Token)
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
                            ( { model | clickedLocation = Nothing }, makeMove model.token model.state from location model.boardSize )

                        False ->
                            ( { model | clickedLocation = Nothing, possibleMoves = Nothing }, Cmd.none )
    in
        case msg of
            Dummy ->
                ( model, Cmd.none )

            InitGameResponse (Err e) ->
                handleErr e

            InitGameResponse (Ok ( t, s, boardSize )) ->
                ( { model | token = t, state = s, boardSize = boardSize }, Cmd.none )

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


type alias Password =
    String


getRequest : Maybe Password -> Maybe Token -> String -> Maybe Encode.Value -> Decode.Decoder a -> Http.Request a
getRequest mpassword mtoken urlToCall encodedVal decoder =
    let
        tryHeader key maybeValue =
            Maybe.map (Http.header key >> List.singleton) maybeValue |> Maybe.withDefault []

        passHeader =
            tryHeader "password" mpassword

        tokenHeader =
            tryHeader "authenticationToken" mtoken

        securityHeaders =
            List.append passHeader tokenHeader
    in
        Http.request
            { method = "POST"
            , headers = securityHeaders
            , url = server ++ urlToCall
            , body = Maybe.map Http.jsonBody encodedVal |> Maybe.withDefault Http.emptyBody
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }


initGame : Password -> Cmd Msg
initGame pswrd =
    let
        url =
            "initGame"

        request =
            getRequest (Just pswrd) Nothing url Nothing Model.initGameDecoder
    in
        Http.send InitGameResponse request


getHint : Token -> GameState -> Cmd Msg
getHint token state =
    let
        url =
            "getHint"

        encoder =
            Encode.object [("state", Model.stateEncoder state)]

        request =
            getRequest Nothing (Just token) url (Just encoder) Model.hintDecoder
    in
        Http.send GetHintResponse request


makeMove : Token -> GameState -> Matrix.Location -> Matrix.Location -> Int -> Cmd Msg
makeMove token state locFrom locTo boardSize =
    let
        url =
            "makeMove"

        encoder =
            Encode.object [("state", Model.stateEncoder state), ("from", Model.locationEncoder locFrom), ("to", Model.locationEncoder locTo)]

        request =
            getRequest Nothing (Just token) url (Just encoder) (Model.gameStateDecoder boardSize)
    in
        Http.send MakeMoveResponse request


getReachablePositions : Token -> GameState -> Matrix.Location -> Cmd Msg
getReachablePositions token state location =
    let
        url =
            "getReachablePositions"

        encoder =
            Encode.object [("state", Model.stateEncoder state), ("location", Model.locationEncoder location)]

        request =
            getRequest Nothing (Just token) url (Just encoder) Model.locationListDecoder
    in
        Http.send GetMovesResponse request


getCurrentScore : Token -> GameState -> Cmd Msg
getCurrentScore token state =
    let
        url =
            "getScore"
        
        encoder =
            Encode.object [("state", Model.stateEncoder state)]

        request =
            getRequest Nothing (Just token) url (Just encoder) Model.scoreDecoder
    in
        Http.send GetScoreResponse request
