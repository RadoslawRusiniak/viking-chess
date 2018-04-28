import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, square, toList, Location, row, col)
import Maybe exposing (withDefault)

import Json.Decode as Decode
import Json.Encode as Encode
import Http

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type Field = Empty | Defender | Attacker | King

type alias Board = Matrix Field
type alias WhoMoves = Int
type alias GameState = (Board, WhoMoves)
type alias Model =
    { state : GameState
    , clickedLocation : Maybe Location
    , possibleMoves : Maybe (List Location)
    , historyPrev : List GameState
    , historyNext : List GameState
    , textareavalue : String
    , errorText : String
    , currentScore : Float
    }

init : (Model, Cmd Msg)
init =
    (Model
        (square 11 (\_ -> Empty), 1)
        Nothing
        Nothing
        []
        []
        ""
        ""
        0
    , getHistory)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- UPDATE

type alias HttpRes a = Result Http.Error a
type alias Move = (Location, Location)

type Msg =
      Clicked Location
    | LoadHistoryViaHttp
    | LoadHistoryResponse (HttpRes (List GameState))
    | UpdateTextAreaValue String
    | LoadHistoryFromTextArea
    | Next | Prev
    | GetScore | GetScoreResponse (HttpRes Float)
    | GetMovesResponse (HttpRes (List Location))
    | MakeMoveResponse (HttpRes GameState)
    | GetHint | GetHintResponse (HttpRes Move)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        handleErr e = ({ model | errorText = toString e}, Cmd.none)

        getHead lst =
            let
                emptyState = (Matrix.square 0 (\_ -> Empty), 0)
            in
                List.head lst |> withDefault emptyState

        getTail lst = List.tail lst |> withDefault []

        insertHistoryIntoModel data = 
            let
                rdata = List.reverse data
            in
                ( {model | historyPrev = getTail rdata, state = getHead rdata, historyNext = [],
                 clickedLocation = Nothing, possibleMoves = Nothing }, Cmd.none)

        zipNext m = {m | historyPrev = m.state :: m.historyPrev, state = getHead m.historyNext,
                         historyNext = getTail m.historyNext}
        zipPrev m = {m | historyPrev = getTail m.historyPrev, state = getHead m.historyPrev,
                         historyNext = m.state :: m.historyNext}

        handleClick : Model -> Location -> (Model, Cmd Msg)
        handleClick model location =
            case model.clickedLocation of
                Nothing -> ( { model | clickedLocation = Just location },
                            getReachablePositions model.state location)
                Just moves -> 
                    case List.member location (model.possibleMoves |> withDefault []) of
                        True -> ( { model | clickedLocation = Nothing }, makeMove model.state location)
                        False -> ( { model | clickedLocation = Nothing, possibleMoves = Nothing }, Cmd.none)
    in
        case msg of
            Clicked location -> handleClick model location
            LoadHistoryViaHttp -> (model, getHistory)
            LoadHistoryResponse (Err e) -> handleErr e
            LoadHistoryResponse (Ok h) -> insertHistoryIntoModel h
            UpdateTextAreaValue v -> ( { model | textareavalue = v }, Cmd.none)
            LoadHistoryFromTextArea -> loadHistoryFromTextArea model.textareavalue |> insertHistoryIntoModel
            Next -> (zipNext model, Cmd.none)
            Prev -> (zipPrev model, Cmd.none)
            GetScore -> (model, getCurrentScore model.state)
            GetScoreResponse (Err e) -> handleErr e
            GetScoreResponse (Ok x) -> ( { model | currentScore = x }, Cmd.none)
            GetMovesResponse (Err e) -> handleErr e
            GetMovesResponse (Ok locs) -> ( { model | possibleMoves = Just locs },
                                            Cmd.none)
            MakeMoveResponse (Err e) -> handleErr e
            MakeMoveResponse (Ok s) -> ( { model | errorText = "", state = s, possibleMoves = Nothing, 
                                         historyPrev = model.state :: model.historyPrev,
                                         historyNext = [] }, Cmd.none)
            GetHint -> (model, getHint model.state)
            GetHintResponse (Err e) -> handleErr e
            GetHintResponse (Ok (from, to)) -> ( { model | clickedLocation = Just from, possibleMoves = Just [to] },
                                                Cmd.none)

server : String
server = "http://localhost:5000/"

getHint : GameState -> Cmd Msg
getHint state = 
    let
        jsonState = stateToJsonValue state |> Encode.encode 0

        moveDecoder : Decode.Decoder Move
        moveDecoder = Decode.map2 ((,)) (Decode.field "from" locationDecoder) (Decode.field "to" locationDecoder)
        hintDecoder = Decode.field "hint" moveDecoder

        url = server ++ "getHint" ++ "?state=" ++ jsonState
        request = Http.get url hintDecoder
    in
        Http.send GetHintResponse request

getHistory : Cmd Msg
getHistory =
    let
        url = server ++ "getHistory"
        request = Http.get url gameStateListDecoder
    in
        Http.send LoadHistoryResponse request

makeMove : GameState -> Location -> Cmd Msg
makeMove state location =
    let
        jsonState = stateToJsonValue state |> Encode.encode 0
        jsonLocation = location |> locationToJsonValue |> Encode.encode 0
        uri = server ++ "makeMove?state=" ++ jsonState ++ "&location=" ++ jsonLocation

        request = Http.get uri gameStateDecoder
    in
        Http.send MakeMoveResponse request

getReachablePositions : GameState -> Location -> Cmd Msg
getReachablePositions state location =
    let
        jsonState = stateToJsonValue state |> Encode.encode 0
        jsonLocation = location |> locationToJsonValue |> Encode.encode 0
        url = server ++ "getReachablePositions?state=" ++ jsonState ++ "&location=" ++ jsonLocation

        listOfMovesDecoder : Decode.Decoder (List Location)
        listOfMovesDecoder = Decode.field "positions" (Decode.list locationDecoder)
        request = Http.get url listOfMovesDecoder
    in
        Http.send GetMovesResponse request

getCurrentScore : GameState -> Cmd Msg
getCurrentScore state =
    let
        jsonVal = stateToJsonValue state
        url = server ++ "getScore?state=" ++ (Encode.encode 0 jsonVal)
        getScoreDecoder : Decode.Decoder Float
        getScoreDecoder = Decode.field "score" Decode.float
        request = Http.get url getScoreDecoder
    in
        Http.send GetScoreResponse request

loadHistoryFromTextArea : String -> List GameState
loadHistoryFromTextArea s = s |> Decode.decodeString gameStateListDecoder |> Result.toMaybe |> withDefault []

locationDecoder : Decode.Decoder Location
locationDecoder = Decode.map2 Matrix.loc (Decode.field "row" Decode.int) (Decode.field "column" Decode.int)

gameStateDecoder : Decode.Decoder GameState
gameStateDecoder =
    let
        charToField c =  case c of
            '.' -> Empty
            'a' -> Attacker
            'd' -> Defender
            'k' -> King
            _ -> Empty
        stringToFields : String -> List Field
        stringToFields s = s |> String.toList |> List.map charToField
        decodeFieldsRow : Decode.Decoder (List Field)
        decodeFieldsRow = Decode.string |> Decode.andThen (\x -> x |> stringToFields |> Decode.succeed)
        decode2DList : Decode.Decoder (List (List Field))
        decode2DList = Decode.list decodeFieldsRow
        decodeBoard : Decode.Decoder Board
        decodeBoard = decode2DList |> Decode.andThen (\l -> l |> Matrix.fromList |> Decode.succeed)
        decodeFieldBoard = Decode.field "board" decodeBoard
        decodeFieldWhoMoves = Decode.field "whoMoves" Decode.int
    in
        Decode.map2 (\a b -> (a,b)) decodeFieldBoard decodeFieldWhoMoves

gameStateListDecoder : Decode.Decoder (List GameState)
gameStateListDecoder = Decode.field "history" (Decode.list gameStateDecoder)

locationToJsonValue : Location -> Encode.Value
locationToJsonValue location = 
    let
        asObject loc = 
            let
                x = Matrix.row loc |> Encode.int
                y = Matrix.col loc |> Encode.int
            in
                Encode.object [("row", x), ("column", y)]
        asLocationObject loc = Encode.object [("location", loc)]
    in
        location |> asObject |> asLocationObject 

stateToJsonValue : GameState -> Encode.Value
stateToJsonValue (b, who) =
    let
        fieldToChar field = case field of
            Empty       -> '.'
            Defender    -> 'd'
            Attacker    -> 'a'
            King        -> 'k'
        toRows : Board -> List String
        toRows b = b |> Matrix.toList |> List.map (\inner -> inner |> List.map fieldToChar |> String.fromList)
        boardToJsonValue b = b |> toRows |> List.map Encode.string |> Encode.list
        whoToJsonValue = Encode.int
    in
        Encode.object [("board", boardToJsonValue b), ("whoMoves", whoToJsonValue who)]

-- VIEW


view : Model -> Html Msg
view model =
    let
        isHighlighted lc = case model.possibleMoves of
            Nothing -> False
            Just moves -> List.member lc moves
        isClicked lc = case model.clickedLocation of
            Nothing -> False
            Just clicked -> lc == clicked
        fieldColors elem location = pickColors elem (isHighlighted location) (isClicked location) 
    in
        div [ style [("display", "flex"), ("flex-direction", "row")] ] [
            div [] (
                model.state |> Tuple.first
                    |> Matrix.mapWithLocation (\loc elem -> div [ myStyle (fieldColors elem loc), onClick (Clicked loc) ] [])
                    |> Matrix.toList
                    |> List.map (div [ style [("height", "56px")]])
            )
            , div [] [
                Html.text ("Now moves: " ++ toString (Tuple.second model.state)), Html.br [] []
                , Html.button [ onClick LoadHistoryViaHttp ] [ text "Load history via http" ]
                , div [] [
                    Html.form [ Html.Events.onSubmit LoadHistoryFromTextArea ] [
                        Html.textarea [ Html.Events.onInput UpdateTextAreaValue ] []
                        ,             
                        button
                            [ Html.Attributes.type_ "submit"
                                , Html.Attributes.disabled False
                            ]
                            [ text "Load history from textarea" ]
                    ]
                ]
                , Html.button [ onClick Prev, Html.Attributes.disabled (List.isEmpty model.historyPrev) ] [ text "prev" ]
                , Html.button [ onClick Next, Html.Attributes.disabled (List.isEmpty model.historyNext) ] [ text "next" ]
                , Html.button [ onClick GetScore ] [ text ("Get score") ]
                , Html.text (toString model.currentScore)
                , Html.button [ onClick GetHint ] [ text ("Get hint") ]
                , div [ errStyle ] [ text model.errorText ]
            ]
        ]

type alias IsHighlighted = Bool
type alias IsClicked = Bool
pickColors : Field -> IsHighlighted -> IsClicked -> (String, String)
pickColors f h c = 
    (
        case f of
            Empty       -> "peru"
            Defender    -> "white"
            Attacker    -> "grey"
            King        -> "purple"
    ,
        if h then "orange" else if c then "red" else "black"
    )

errStyle : Attribute Msg
errStyle = style [
      ("height", "100px")
    , ("width", "100px")
    ]

myStyle : (String, String) -> Attribute Msg
myStyle (background, border) =
    style
        [ ("backgroundColor", background)
        , ("height", "50px")
        , ("width", "50px")
        , ("border", "3px solid " ++ border)
        , ("display", "inline-block")
        ]
