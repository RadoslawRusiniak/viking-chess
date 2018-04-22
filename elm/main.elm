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
type alias Model =
    { board : Board
    , clickedLocation : Maybe Location
    , possibleMoves : Maybe (List Location)
    , historyPrev : List Board
    , historyNext : List Board
    , textareavalue : String
    , errorText : String
    , currentScore : String
    }

init : (Model, Cmd Msg)
init =
    (Model
        (square 11 (\_ -> Empty))
        Nothing
        Nothing
        []
        []
        ""
        ""
        ""
    , getHistory)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- UPDATE

type alias HttpRes a = Result Http.Error a

type Msg =
      Clicked Location
    | LoadHistoryViaHttp
    | LoadHistoryResponse (HttpRes (List Board))
    | UpdateTextAreaValue String
    | LoadHistoryFromTextArea
    | Next | Prev
    | GetScore | GetScoreAnswer (HttpRes String)
    | GetMovesResponse (HttpRes (List Location))
    | MakeMoveResponse (HttpRes Board)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        handleErr e = ({ model | errorText = toString e}, Cmd.none)

        getHead lst =
            let
                emptyBoard = Matrix.square 0 (\_ -> Empty)
            in
                List.head lst |> withDefault emptyBoard

        getTail lst = List.tail lst |> withDefault []

        insertHistoryIntoModel data = 
            let
                rdata = List.reverse data
            in
                ( {model | historyPrev = getTail rdata, board = getHead rdata, historyNext = [],
                 clickedLocation = Nothing, possibleMoves = Nothing }, Cmd.none)

        zipNext m = {m | historyPrev = m.board :: m.historyPrev, board = getHead m.historyNext,
                         historyNext = getTail m.historyNext}
        zipPrev m = {m | historyPrev = getTail m.historyPrev, board = getHead m.historyPrev,
                         historyNext = m.board :: m.historyNext}

        handleClick : Model -> Location -> (Model, Cmd Msg)
        handleClick model location =
            case model.clickedLocation of
                Nothing -> ( { model | clickedLocation = Just location }, getReachablePositions model.board location)
                Just moves -> 
                    case List.member location (model.possibleMoves |> withDefault []) of
                        True -> ( { model | clickedLocation = Nothing }, makeMove model.board location)
                        False -> ( { model | errorText = "Wrong move" }, Cmd.none)
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
            GetScore -> (model, getCurrentScore model)
            GetScoreAnswer (Err e) -> handleErr e
            GetScoreAnswer (Ok x) -> ( { model | currentScore = x }, Cmd.none)
            GetMovesResponse (Err e) -> handleErr e
            GetMovesResponse (Ok locs) -> ( { model | board = model.board, possibleMoves = Just locs },
                                            Cmd.none)
            MakeMoveResponse (Err e) -> handleErr e
            MakeMoveResponse (Ok b) -> ( { model | errorText = "", board = b, possibleMoves = Nothing, 
                                         historyPrev = model.board :: model.historyPrev,
                                         historyNext = [] }, Cmd.none)

server : String
server = "http://localhost:5000/"

getHistory : Cmd Msg
getHistory =
    let
        url = server ++ "getHistory"
        request = Http.get url boardListDecoder
    in
        Http.send LoadHistoryResponse request

makeMove : Board -> Location -> Cmd Msg
makeMove board location =
    let
        jsonBoard = board |> boardToJsonValue |> Encode.encode 0
        jsonLocation = location |> locationToJsonValue |> Encode.encode 0
        uri = server ++ "makeMove?board=" ++ jsonBoard ++ "&location=" ++ jsonLocation

        request = Http.get uri boardDecoder
    in
        Http.send MakeMoveResponse request

getReachablePositions : Board -> Location -> Cmd Msg
getReachablePositions board location =
    let
        jsonBoard = board |> boardToJsonValue |> Encode.encode 0
        jsonLocation = location |> locationToJsonValue |> Encode.encode 0
        url = server ++ "getReachablePositions?board=" ++ jsonBoard ++ "&location=" ++ jsonLocation

        locationDecoder : Decode.Decoder Location
        locationDecoder = Decode.map2 Matrix.loc (Decode.field "row" Decode.int) (Decode.field "column" Decode.int)

        listOfMovesDecoder : Decode.Decoder (List Location)
        listOfMovesDecoder = Decode.field "positions" (Decode.list locationDecoder)
        request = Http.get url listOfMovesDecoder
    in
        Http.send GetMovesResponse request

getCurrentScore : Model -> Cmd Msg
getCurrentScore model =
    let
        jsonVal = boardToJsonValue model.board
        url = server ++ "getScore?board=" ++ (Encode.encode 0 jsonVal)
        getScoreDecoder : Decode.Decoder String
        getScoreDecoder = Decode.string
        request = Http.get url getScoreDecoder
    in
        Http.send GetScoreAnswer request

loadHistoryFromTextArea : String -> List Board
loadHistoryFromTextArea s = s |> Decode.decodeString boardListDecoder |> Result.toMaybe |> withDefault []

boardDecoder : Decode.Decoder Board
boardDecoder =
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
    in
        decode2DList |> Decode.andThen (\l -> l |> Matrix.fromList |> Decode.succeed)

boardListDecoder : Decode.Decoder (List Board)
boardListDecoder = Decode.list boardDecoder

locationToJsonValue : Location -> Encode.Value
locationToJsonValue location = 
    let
        x = Matrix.row location
        y = Matrix.col location
    in
        Encode.list [Encode.int x, Encode.int y]

boardToJsonValue : Board -> Encode.Value
boardToJsonValue b =
    let
        fieldToChar field = case field of
            Empty       -> '.'
            Defender    -> 'd'
            Attacker    -> 'a'
            King        -> 'k'
        toRows : Board -> List String
        toRows b = b |> Matrix.toList |> List.map (\inner -> inner |> List.map fieldToChar |> String.fromList)
    in
        b |> toRows |> List.map Encode.string |> Encode.list


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
                model.board
                    |> Matrix.mapWithLocation (\loc elem -> div [ myStyle (fieldColors elem loc), onClick (Clicked loc) ] [])
                    |> Matrix.toList
                    |> List.map (div [ style [("height", "56px")]])
            )
            , div [] [
                Html.button [ onClick LoadHistoryViaHttp ] [ text "Load history via http" ]
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
                , Html.text model.currentScore
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
