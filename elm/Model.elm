module Model
    exposing
        ( Model
        , Mode(..)
        , Positioning
        , Pawn(..)
        , Move
        , GameState
        , WhoMoves
        , toString
        , otherSide
        , isSameSide
        , Token
        , emptyModel
        , emptyState
        , emptyPositioning
        , initGameDecoder
        , gameStateDecoder
        , locationDecoder
        , locationListDecoder
        , hintDecoder
        , scoreDecoder
        , locationEncoder
        , stateEncoder
        )

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (withDefault)
import List.Split exposing (chunksOfLeft)
import Matrix exposing (Matrix, Location)


type Mode
    = Game
    | Edit


type WhoMoves
    = WAttacker
    | WDefender


toString : WhoMoves -> String
toString who =
    case who of
        WAttacker ->
            "Attacker"

        WDefender ->
            "Defender"


otherSide : WhoMoves -> WhoMoves
otherSide who =
    case who of
        WAttacker ->
            WDefender

        WDefender ->
            WAttacker


type Pawn
    = Attacker
    | Defender
    | King


isSameSide : WhoMoves -> Pawn -> Bool
isSameSide whosTurn pawn =
    (whosTurn == WAttacker && pawn == Attacker) || (whosTurn == WDefender && (pawn == Defender || pawn == King))


type alias Positioning =
    Matrix (Maybe Pawn)


type alias GameState =
    ( Positioning, WhoMoves )


type alias Move =
    ( Matrix.Location, Matrix.Location )


type alias Token =
    String


type alias Model =
    { mode : Mode
    , boardSize : Int
    , state : GameState
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
        Game
        0
        emptyState
        Nothing
        Nothing
        Nothing
        []
        []
        ""
        0
        ""


emptyPositioning : Positioning
emptyPositioning =
    Matrix.square 11 (always Nothing)


emptyState : GameState
emptyState =
    ( emptyPositioning, WAttacker )


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
        charToPawn c =
            case c of
                '.' ->
                    Nothing

                'a' ->
                    Just Attacker

                'd' ->
                    Just Defender

                'k' ->
                    Just King

                _ ->
                    Nothing

        decodePawns2DList : Decode.Decoder (List (List (Maybe Pawn)))
        decodePawns2DList =
            let
                toRows =
                    chunksOfLeft boardSize

                stringTo2DPositions =
                    String.toList >> List.map charToPawn >> toRows
            in
                Decode.string |> Decode.andThen (stringTo2DPositions >> Decode.succeed)

        decodePositioning : Decode.Decoder Positioning
        decodePositioning =
            decodePawns2DList |> Decode.andThen (Matrix.fromList >> Decode.succeed)

        decodeFieldPositioning =
            Decode.field "board" decodePositioning

        intToWhosTurn x =
            case x of
                0 ->
                    WAttacker

                1 ->
                    WDefender

                --TODO use never somehow
                _ ->
                    WAttacker

        decodeFieldWhoMoves =
            Decode.field "whoMoves" Decode.int |> Decode.andThen (intToWhosTurn >> Decode.succeed)
    in
        Decode.map2 (,) decodeFieldPositioning decodeFieldWhoMoves


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
        pawnToChar pos =
            case pos of
                Nothing ->
                    '.'

                Just Defender ->
                    'd'

                Just Attacker ->
                    'a'

                Just King ->
                    'k'

        positionsToJsonValue =
            Matrix.toList >> List.concat >> List.map pawnToChar >> String.fromList >> Encode.string

        whosTurnToInt side =
            case side of
                WAttacker ->
                    0

                WDefender ->
                    1

        whoToJsonValue =
            whosTurnToInt >> Encode.int
    in
        Encode.object [ ( "board", positionsToJsonValue b ), ( "whoMoves", whoToJsonValue who ) ]
