module Main exposing (main)

import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Http
import Matrix
import Navigation as Nav exposing (program, Location)
import UrlParser exposing (Parser, top, s, (<?>), stringParam, parsePath)
import Model exposing (Msg(..), Model, GameState, Board, Field(..), Move, WhoMoves)


main : Program Never Model Msg
main =
    Nav.program
        --TODO how to ignore
        (\_ -> Dummy)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    let
        --TODO how to get rid of "main.elm"
        parser : Parser (Maybe String -> a) a
        parser =
            s "main.elm" <?> stringParam "password"

        parsePassword =
            parsePath parser >> withDefault Nothing >> withDefault "wrong"
    in
        ( Model.Model
            Model.emptyState
            Nothing
            Nothing
            Nothing
            []
            []
            ""
            0
            ""
        , initGame <| parsePassword location
        )



-- UPDATE


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



-- VIEW


view : Model -> Html Msg
view model =
    let
        isHighlighted lc =
            case model.possibleMoves of
                Nothing ->
                    False

                Just moves ->
                    List.member lc moves

        isClicked lc =
            case model.clickedLocation of
                Nothing ->
                    False

                Just clicked ->
                    lc == clicked

        isPartOfHint lc =
            case model.hint of
                Nothing ->
                    False

                Just ( from, to ) ->
                    lc == from || lc == to

        getFieldStyle loc =
            fieldStyle (isHighlighted loc) (isClicked loc) (isPartOfHint loc)

        --TODO maybe nicer way of handling empty field
        setPawn elem =
            if Model.isEmptyField elem then
                div [] []
            else
                div [ pawnStyle elem ] []
    in
        div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
            [ div []
                (model.state
                    |> Tuple.first
                    |> Matrix.mapWithLocation (\loc elem -> div [ getFieldStyle loc, onClick (Clicked loc) ] [ setPawn elem ])
                    |> Matrix.toList
                    |> List.map (div [ style [ ( "height", "56px" ) ] ])
                )
            , div []
                [ Html.text ("Now moves: " ++ toString (Tuple.second model.state))
                , Html.br [] []
                , Html.button [ onClick Prev, Html.Attributes.disabled (List.isEmpty model.historyPrev) ] [ text "prev" ]
                , Html.button [ onClick Next, Html.Attributes.disabled (List.isEmpty model.historyNext) ] [ text "next" ]
                , Html.button [ onClick GetScore ] [ text ("Get score") ]
                , Html.text (toString model.currentScore)
                , Html.button [ onClick GetHint ] [ text ("Get hint") ]
                , div [ errStyle ] [ text model.errorText ]
                ]
            ]


errStyle : Attribute Msg
errStyle =
    style
        [ ( "height", "100px" )
        , ( "width", "100px" )
        ]


type alias IsHighlighted =
    Bool


type alias IsClicked =
    Bool


type alias IsPartOfHint =
    Bool


fieldStyle : IsHighlighted -> IsClicked -> IsPartOfHint -> Attribute Msg
fieldStyle highlighted clicked hint =
    let
        background =
            if hint then
                "green"
            else if highlighted then
                "orange"
            else if clicked then
                "red"
            else
                "peru"
    in
        style
            [ ( "backgroundColor", background )
            , ( "height", "50px" )
            , ( "width", "50px" )
            , ( "border", "3px solid black" )
            , ( "display", "inline-block" )
            ]


pawnStyle : Field -> Attribute Msg
pawnStyle f =
    let
        representationColor f =
            case f of
                Empty ->
                    "peru"

                Defender ->
                    "white"

                Attacker ->
                    "grey"

                King ->
                    "purple"
    in
        style
            [ ( "backgroundColor", representationColor f )
            , ( "height", "44px" )
            , ( "width", "44px" )
            , ( "margin-left", "3px" )
            , ( "margin-top", "3px" )
            , ( "display", "inline-block" )
            , ( "position", "absolute" )
            , ( "-webkit-border-radius", "44px" )
            ]
