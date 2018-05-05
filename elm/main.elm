module Main exposing (main, Model)

import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Matrix as Mtrx exposing (Matrix, square, toList, Location, row, col)
import Navigation as Nav exposing (program, Location)
import UrlParser exposing (Parser, top, s, (<?>), stringParam, parsePath)
import List.Split exposing (chunksOfLeft)


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



-- MODEL


type Field
    = Empty
    | Defender
    | Attacker
    | King


type alias Board =
    Matrix Field


type alias WhoMoves =
    Int


type alias GameState =
    ( Board, WhoMoves )


type alias Model =
    { state : GameState
    , clickedLocation : Maybe Mtrx.Location
    , possibleMoves : Maybe (List Mtrx.Location)
    , historyPrev : List GameState
    , historyNext : List GameState
    , errorText : String
    , currentScore : Float
    , token : String
    }


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    let
        --TODO how to get rid of "main.elm"
        parser : Parser (Maybe String -> a) a
        parser =
            s "main.elm" <?> stringParam "password"

        parsedPassword =
            parsePath parser location |> withDefault Nothing |> withDefault "wrong"
    in
        ( Model
            ( square 0 (\_ -> Empty), 1 )
            Nothing
            Nothing
            []
            []
            ""
            0
            ""
        , initGame parsedPassword
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type alias HttpRes a =
    Result Http.Error a


type alias Move =
    ( Mtrx.Location, Mtrx.Location )


type Msg
    = Dummy
    | InitGameResponse (HttpRes ( String, GameState ))
    | Clicked Mtrx.Location
    | Next
    | Prev
    | GetScore
    | GetScoreResponse (HttpRes Float)
    | GetMovesResponse (HttpRes (List Mtrx.Location))
    | MakeMoveResponse (HttpRes GameState)
    | GetHint
    | GetHintResponse (HttpRes Move)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        handleErr e =
            ( { model | errorText = toString e }, Cmd.none )

        getHead lst =
            let
                emptyState =
                    ( Mtrx.square 0 (\_ -> Empty), 0 )
            in
                List.head lst |> withDefault emptyState

        getTail lst =
            List.tail lst |> withDefault []

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

        handleClick : Model -> Mtrx.Location -> ( Model, Cmd Msg )
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
                ( { model | possibleMoves = Just locs }
                , Cmd.none
                )

            MakeMoveResponse (Err e) ->
                handleErr e

            MakeMoveResponse (Ok s) ->
                ( { model
                    | errorText = ""
                    , state = s
                    , possibleMoves = Nothing
                    , historyPrev = model.state :: model.historyPrev
                    , historyNext = []
                  }
                , Cmd.none
                )

            GetHint ->
                ( model, getHint model.token model.state )

            GetHintResponse (Err e) ->
                handleErr e

            GetHintResponse (Ok ( from, to )) ->
                ( { model | clickedLocation = Just from, possibleMoves = Just [ to ] }
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
                , expect = Http.expectJson initGameDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send InitGameResponse request


getHint : String -> GameState -> Cmd Msg
getHint token state =
    let
        jsonState =
            stateToJsonValue state |> Encode.encode 0

        moveDecoder : Decode.Decoder Move
        moveDecoder =
            Decode.map2 ((,)) (Decode.field "from" locationDecoder) (Decode.field "to" locationDecoder)

        hintDecoder =
            Decode.field "hint" moveDecoder

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url = server ++ "getHint" ++ "?state=" ++ jsonState
                , body = Http.emptyBody
                , expect = Http.expectJson hintDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send GetHintResponse request


makeMove : String -> GameState -> Mtrx.Location -> Mtrx.Location -> Cmd Msg
makeMove token state locFrom locTo =
    let
        jsonState =
            stateToJsonValue state |> Encode.encode 0

        jsonLocation loc =
            loc |> locationToJsonValue |> Encode.encode 0

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url =
                    server
                        ++ "makeMove?state="
                        ++ jsonState
                        ++ "&from="
                        ++ jsonLocation locFrom
                        ++ "&to="
                        ++ jsonLocation locTo
                , body = Http.emptyBody
                , expect = Http.expectJson gameStateDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send MakeMoveResponse request


getReachablePositions : String -> GameState -> Mtrx.Location -> Cmd Msg
getReachablePositions token state location =
    let
        jsonState =
            stateToJsonValue state |> Encode.encode 0

        jsonLocation =
            location |> locationToJsonValue |> Encode.encode 0

        listOfMovesDecoder : Decode.Decoder (List Mtrx.Location)
        listOfMovesDecoder =
            Decode.field "positions" (Decode.list locationDecoder)

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url = server ++ "getReachablePositions?state=" ++ jsonState ++ "&location=" ++ jsonLocation
                , body = Http.emptyBody
                , expect = Http.expectJson listOfMovesDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send GetMovesResponse request


getCurrentScore : String -> GameState -> Cmd Msg
getCurrentScore token state =
    let
        jsonVal =
            stateToJsonValue state

        getScoreDecoder : Decode.Decoder Float
        getScoreDecoder =
            Decode.field "score" Decode.float

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "authenticationToken" token ]
                , url = server ++ "getScore?state=" ++ (Encode.encode 0 jsonVal)
                , body = Http.emptyBody
                , expect = Http.expectJson getScoreDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send GetScoreResponse request


locationDecoder : Decode.Decoder Mtrx.Location
locationDecoder =
    Decode.map2 Mtrx.loc (Decode.field "row" Decode.int) (Decode.field "column" Decode.int)


gameStateDecoder : Decode.Decoder GameState
gameStateDecoder =
    let
        charToField c =
            case c of
                '.' ->
                    Empty

                'a' ->
                    Attacker

                'd' ->
                    Defender

                'k' ->
                    King

                _ ->
                    Empty

        --TODO pass board size and read it here
        boardLen =
            11

        stringToFields : List Char -> List Field
        stringToFields =
            List.map charToField

        strings : String -> List (List Char)
        strings s =
            String.toList s |> chunksOfLeft boardLen

        decodeFields : Decode.Decoder (List (List Field))
        decodeFields =
            Decode.string |> Decode.andThen (\x -> x |> strings |> List.map stringToFields |> Decode.succeed)

        decodeBoard : Decode.Decoder Board
        decodeBoard =
            decodeFields |> Decode.andThen (\l -> l |> Mtrx.fromList |> Decode.succeed)

        decodeFieldBoard =
            Decode.field "board" decodeBoard

        decodeFieldWhoMoves =
            Decode.field "whoMoves" Decode.int
    in
        Decode.map2 (\a b -> ( a, b )) decodeFieldBoard decodeFieldWhoMoves


initGameDecoder : Decode.Decoder ( String, GameState )
initGameDecoder =
    let
        tokenDecoder =
            Decode.field "token" Decode.string
    in
        Decode.map2 (\a b -> ( a, b )) tokenDecoder gameStateDecoder


locationToJsonValue : Mtrx.Location -> Encode.Value
locationToJsonValue location =
    let
        asObject loc =
            let
                x =
                    Mtrx.row loc |> Encode.int

                y =
                    Mtrx.col loc |> Encode.int
            in
                Encode.object [ ( "row", x ), ( "column", y ) ]

        asLocationObject loc =
            Encode.object [ ( "location", loc ) ]
    in
        location |> asObject |> asLocationObject


stateToJsonValue : GameState -> Encode.Value
stateToJsonValue ( b, who ) =
    let
        fieldToChar field =
            case field of
                Empty ->
                    '.'

                Defender ->
                    'd'

                Attacker ->
                    'a'

                King ->
                    'k'

        toRows : Board -> List String
        toRows b =
            b |> Mtrx.toList |> List.map (\inner -> inner |> List.map fieldToChar |> String.fromList)

        boardToJsonValue b =
            b |> toRows |> List.map Encode.string |> Encode.list

        whoToJsonValue =
            Encode.int
    in
        Encode.object [ ( "board", boardToJsonValue b ), ( "whoMoves", whoToJsonValue who ) ]



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

        getFieldStyle loc =
            fieldStyle (isHighlighted loc) (isClicked loc)

        --TODO maybe nicer way of handling empty field
        setPawn elem =
            if elem == Empty then
                div [] []
            else
                div [ pawnStyle elem ] []
    in
        div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
            [ div []
                (model.state
                    |> Tuple.first
                    |> Mtrx.mapWithLocation (\loc elem -> div [ getFieldStyle loc, onClick (Clicked loc) ] [ setPawn elem ])
                    |> Mtrx.toList
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


type alias IsHighlighted =
    Bool


type alias IsClicked =
    Bool


errStyle : Attribute Msg
errStyle =
    style
        [ ( "height", "100px" )
        , ( "width", "100px" )
        ]


fieldStyle : IsHighlighted -> IsClicked -> Attribute Msg
fieldStyle h c =
    let
        background =
            if h then
                "orange"
            else if c then
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
        background =
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
            [ ( "backgroundColor", background )
            , ( "height", "44px" )
            , ( "width", "44px" )
            , ( "margin-left", "3px" )
            , ( "margin-top", "3px" )
            , ( "display", "inline-block" )
            , ( "position", "absolute" )
            , ( "-webkit-border-radius", "44px" )
            ]
