module Model
    exposing
        ( Model
        , Board
        , Field
        , Move
        , GameState
        , WhoMoves
        , emptyState
        , isEmptyField
        , initGameDecoder
        , gameStateDecoder
        , locationDecoder
        , locationEncoder
        , stateEncoder
        , representationColor
        )

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


type alias Model =
    { state : GameState
    , clickedLocation : Maybe Matrix.Location
    , possibleMoves : Maybe (List Matrix.Location)
    , hint : Maybe Move
    , historyPrev : List GameState
    , historyNext : List GameState
    , errorText : String
    , currentScore : Float
    , token : String
    }


emptyState : GameState
emptyState =
    let
        emptyBoard =
            Matrix.square 2 (\_ -> Empty)
    in
        ( emptyBoard, 0 )


isEmptyField : Field -> Bool
isEmptyField =
    (==) Empty


locationDecoder : Decode.Decoder Matrix.Location
locationDecoder =
    Decode.map2 Matrix.loc (Decode.field "row" Decode.int) (Decode.field "column" Decode.int)


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
            decodeFields |> Decode.andThen (\l -> l |> Matrix.fromList |> Decode.succeed)

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


locationEncoder : Matrix.Location -> Encode.Value
locationEncoder location =
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
        location |> asObject |> asLocationObject


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

        toRows : Board -> List String
        toRows b =
            b |> Matrix.toList |> List.map (\inner -> inner |> List.map fieldToChar |> String.fromList)

        boardToJsonValue b =
            b |> toRows |> List.map Encode.string |> Encode.list

        whoToJsonValue =
            Encode.int
    in
        Encode.object [ ( "board", boardToJsonValue b ), ( "whoMoves", whoToJsonValue who ) ]



--TODO probably This shouldn't be in model


representationColor : Field -> String
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
