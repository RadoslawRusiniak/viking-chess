module Update exposing (init, update)

import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Maybe exposing (withDefault)
import Matrix
import Messages exposing (Msg(..))
import Model exposing (Model, Pawn(..), GameState, Token)
--import Navigation
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((<?>))
import Url.Parser.Query as UrlParserQuery


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        handleErr e =
            ( { model | errorText = Debug.toString e }, Cmd.none )

        getHead =
            List.head >> withDefault Model.emptyState

        getTail =
            List.tail >> withDefault []

        zipNext m =
            { m
                | historyPrev = m.state :: m.historyPrev
                , state = getHead m.historyNext
                , historyNext = getTail m.historyNext
                , possibleMoves = Nothing
            }

        zipPrev m =
            { m
                | historyPrev = getTail m.historyPrev
                , state = getHead m.historyPrev
                , historyNext = m.state :: m.historyNext
                , possibleMoves = Nothing
            }

        handleGameClick : Model -> Matrix.Location -> ( Model, Cmd Msg )
        handleGameClick cmodel location =
            case cmodel.clickedLocation of
                Nothing ->
                    let
                        whosTurn =
                            Tuple.second cmodel.state

                        mpawn =
                            Tuple.first cmodel.state |> Matrix.get location |> Maybe.withDefault Nothing

                        sameSide =
                            Maybe.map (Model.isSameSide whosTurn) mpawn |> Maybe.withDefault False
                    in
                        if sameSide then
                            ( { cmodel | clickedLocation = Just location }
                            , getReachablePositions cmodel.token cmodel.state location
                            )
                        else
                            ( { cmodel | clickedLocation = Just location }
                            , Cmd.none
                            )

                Just from ->
                    case List.member location (cmodel.possibleMoves |> withDefault []) of
                        True ->
                            ( { cmodel | clickedLocation = Nothing }, makeMove cmodel.token cmodel.state from location cmodel.boardSize )

                        False ->
                            ( { cmodel | clickedLocation = Nothing, possibleMoves = Nothing }, Cmd.none )

        handleEditClick : Model -> Matrix.Location -> ( Model, Cmd Msg )
        handleEditClick cmodel location =
            let
                positions =
                    Tuple.first cmodel.state

                rotatePawn : Maybe Pawn -> Maybe Pawn
                rotatePawn p =
                    case p of
                        Nothing ->
                            Just Attacker

                        Just Attacker ->
                            Just Defender

                        Just Defender ->
                            Just King

                        Just King ->
                            Nothing

                updatePawns inlocation =
                    Matrix.update inlocation rotatePawn

                updatePositioning inlocation =
                    Tuple.mapFirst (updatePawns inlocation)
            in
                ( { cmodel | state = updatePositioning location cmodel.state }, Cmd.none )
    in
        case msg of
            Dummy ->
                ( model, Cmd.none )

            InitGameResponse (Err e) ->
                handleErr e

            InitGameResponse (Ok ( t, s, boardSize )) ->
                ( { model | token = t, state = s, boardSize = boardSize }, Cmd.none )

            Clicked location ->
                if model.mode == Model.Game then
                    handleGameClick model location
                else
                    handleEditClick model location

            Next ->
                let
                    newmodel =
                        zipNext model
                in
                    ( newmodel, updateState newmodel.token newmodel.state )

            Prev ->
                let
                    newmodel =
                        zipPrev model
                in
                    ( newmodel, updateState newmodel.token newmodel.state )

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

            ChangeSide ->
                let
                    newstate =
                        Tuple.mapSecond Model.otherSide model.state
                in
                    ( { model | state = newstate }, updateState model.token newstate )

            EditPosition ->
                ( { model | mode = Model.Edit }, Cmd.none )

            ClearPawns ->
                let
                    newstate =
                        Tuple.mapFirst (always Model.emptyPositioning) model.state
                in
                    ( { model | state = newstate }, Cmd.none )

            FinishEditing ->
                ( { model | mode = Model.Game }, updateState model.token model.state )

            UpdateStateResponse (Err e) ->
                handleErr e

            UpdateStateResponse (Ok ()) ->
                ( model, Cmd.none )

            LinkClicked _ ->
                ( model, Cmd.none )

            UrlChanged _ ->
                ( model, Cmd.none )


init : Url -> ( Model, Cmd Msg )
init location =
    let
        --TODO how to get rid of "main.elm"
        parser : UrlParser.Parser (Maybe String -> a) a
        parser =
            UrlParser.s "main.elm" <?> UrlParserQuery.string "password"

        parsePassword =
            UrlParser.parse parser >> withDefault Nothing >> withDefault "wrong"
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


updateState : Token -> GameState -> Cmd Msg
updateState token state =
    let
        url =
            "updateState"

        encoder =
            Encode.object [ ( "state", Model.stateEncoder state ) ]

        request =
            getRequest Nothing (Just token) url (Just encoder) (Decode.succeed ())
    in
        Http.send UpdateStateResponse request


getHint : Token -> GameState -> Cmd Msg
getHint token state =
    let
        url =
            "getHint"

        encoder =
            Encode.object [ ( "state", Model.stateEncoder state ) ]

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
            Encode.object [ ( "state", Model.stateEncoder state ), ( "from", Model.locationEncoder locFrom ), ( "to", Model.locationEncoder locTo ) ]

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
            Encode.object [ ( "state", Model.stateEncoder state ), ( "location", Model.locationEncoder location ) ]

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
            Encode.object [ ( "state", Model.stateEncoder state ) ]

        request =
            getRequest Nothing (Just token) url (Just encoder) Model.scoreDecoder
    in
        Http.send GetScoreResponse request
