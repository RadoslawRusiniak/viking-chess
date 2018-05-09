module Model
    exposing
        ( Msg(..)
        , Model
        , Board
        , Field(..)
        , Move
        , GameState
        , WhoMoves
        , Token
        , emptyModel
        , emptyState
        , isEmptyField
        , initGameDecoder
        , gameStateDecoder
        , locationDecoder
        , locationListDecoder
        , hintDecoder
        , scoreDecoder
        , locationEncoder
        , stateEncoder
        )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (withDefault)
import List.Split exposing (chunksOfLeft)
import Matrix exposing (Matrix, Location)


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


type alias Move =
    ( Matrix.Location, Matrix.Location )


type alias Token =
    String


type alias Model =
    { state : GameState
    , boardSize : Int
    , clickedLocation : Maybe Matrix.Location
    , possibleMoves : Maybe (List Matrix.Location)
    , hint : Maybe Move
    , historyPrev : List GameState
    , historyNext : List GameState
    , errorText : String
    , currentScore : Float
    , token : Token
    }


emptyModel : Model
emptyModel =
    Model
        emptyState
        0
        Nothing
        Nothing
        Nothing
        []
        []
        ""
        0
        ""


emptyState : GameState
emptyState =
    let
        emptyBoard =
            Matrix.square 2 (always Empty)
    in
        ( emptyBoard, 0 )


isEmptyField : Field -> Bool
isEmptyField =
    (==) Empty


type alias HttpRes a =
    Result Http.Error a


type Msg
    = Dummy
    | InitGameResponse (HttpRes ( Token, GameState, Int ))
    | Clicked Matrix.Location
    | Next
    | Prev
    | GetScore
    | GetScoreResponse (HttpRes Float)
    | GetMovesResponse (HttpRes (List Matrix.Location))
    | MakeMoveResponse (HttpRes GameState)
    | GetHint
    | GetHintResponse (HttpRes Move)


locationDecoder : Decode.Decoder Matrix.Location
locationDecoder =
    Decode.map2 Matrix.loc (Decode.field "row" Decode.int) (Decode.field "column" Decode.int)


moveDecoder : Decode.Decoder Move
moveDecoder =
    Decode.map2 (,) (Decode.field "from" locationDecoder) (Decode.field "to" locationDecoder)


locationListDecoder : Decode.Decoder (List Matrix.Location)
locationListDecoder =
    Decode.field "positions" (Decode.list locationDecoder)


hintDecoder : Decode.Decoder Move
hintDecoder =
    Decode.field "hint" moveDecoder


gameStateDecoder : Int -> Decode.Decoder GameState
gameStateDecoder boardSize =
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

        decodeFields : Decode.Decoder (List (List Field))
        decodeFields =
            let
                toFields =
                    List.map charToField

                toRows =
                    chunksOfLeft boardSize

                stringTo2DFields =
                    String.toList >> toFields >> toRows
            in
                Decode.string |> Decode.andThen (stringTo2DFields >> Decode.succeed)

        decodeBoard : Decode.Decoder Board
        decodeBoard =
            decodeFields |> Decode.andThen (Matrix.fromList >> Decode.succeed)

        decodeFieldBoard =
            Decode.field "board" decodeBoard

        decodeFieldWhoMoves =
            Decode.field "whoMoves" Decode.int
    in
        Decode.map2 (,) decodeFieldBoard decodeFieldWhoMoves


initGameDecoder : Decode.Decoder ( Token, GameState, Int )
initGameDecoder =
    let
        tokenDecoder =
            Decode.field "token" Decode.string

        boardSizeDecoder =
            Decode.field "boardSize" Decode.int

        initDecoder : Int -> Decode.Decoder ( Token, GameState )
        initDecoder boardSize =
            Decode.map2 (,) tokenDecoder (gameStateDecoder boardSize)

        appendSize : Int -> ( Token, GameState ) -> Decode.Decoder ( Token, GameState, Int )
        appendSize boardSize ( t, st ) =
            ( t, st, boardSize ) |> Decode.succeed

        finalDecoder : Int -> Decode.Decoder ( Token, GameState, Int )
        finalDecoder boardSize =
            Decode.andThen (appendSize boardSize) (initDecoder boardSize)
    in
        boardSizeDecoder |> Decode.andThen finalDecoder


scoreDecoder : Decode.Decoder Float
scoreDecoder =
    Decode.field "score" Decode.float


locationEncoder : Matrix.Location -> Encode.Value
locationEncoder =
    let
        asObject loc =
            let
                x =
                    Matrix.row loc |> Encode.int

                y =
                    Matrix.col loc |> Encode.int
            in
                Encode.object [ ( "row", x ), ( "column", y ) ]

        asLocationObject loc =
            Encode.object [ ( "location", loc ) ]
    in
        asObject >> asLocationObject


stateEncoder : GameState -> Encode.Value
stateEncoder ( b, who ) =
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

        boardToJsonValue =
            Matrix.toList >> List.concat >> List.map fieldToChar >> String.fromList >> Encode.string

        whoToJsonValue =
            Encode.int
    in
        Encode.object [ ( "board", boardToJsonValue b ), ( "whoMoves", whoToJsonValue who ) ]