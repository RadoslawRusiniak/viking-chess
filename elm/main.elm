import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, square, toList, Location, row, col)
import Maybe exposing (withDefault)

import Json.Decode as Decode
import Json.Encode as Encode
import Http

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type Field = Empty | Highlighted | White | Black | King

type alias Board = Matrix Field
type alias Model =
    { board : Board
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
        []
        []
        ""
        ""
        ""
    , getBoard)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- UPDATE

type alias HttpRes a = Result Http.Error a

type Msg =
      Clicked Location
    | LoadHistoryViaHttp
    | NewHistory (HttpRes (List Board))
    | UpdateTextAreaValue String
    | LoadHistoryFromTextArea
    | Next | Prev
    | GetBoardClicked
    | GetScore | GetScoreAnswer (HttpRes String)
    | BoardResponse (HttpRes Board)
    | GetMovesResponse (HttpRes (List Location))
    | MakeMoveResponse (HttpRes Board)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        handleErr e = ({ model | errorText = toString e}, Cmd.none)
        emptyBoard = Matrix.square 0 (\_ -> Empty)
        insertHistoryIntoModel data = ({model | historyPrev = [], board = List.head data |> withDefault emptyBoard, historyNext = List.tail data |> withDefault []}, Cmd.none)
    in
        case msg of
            Clicked location -> (model, handleClick model location)
            LoadHistoryViaHttp -> (model, getHistory)
            NewHistory (Ok h) -> insertHistoryIntoModel h
            NewHistory (Err e) -> handleErr e
            UpdateTextAreaValue v -> ( { model | textareavalue = v}, Cmd.none)
            LoadHistoryFromTextArea -> loadHistoryFromTextArea model.textareavalue |> insertHistoryIntoModel
            Next -> ({model | historyPrev = model.board :: model.historyPrev, board = List.head model.historyNext |> withDefault emptyBoard, historyNext = List.tail model.historyNext |> withDefault []}, Cmd.none)
            Prev -> ({model | historyPrev = List.tail model.historyPrev |> withDefault [], board = List.head model.historyPrev |> withDefault emptyBoard, historyNext = model.board :: model.historyNext}, Cmd.none)
            GetBoardClicked -> (model, getBoard)
            GetScore -> (model, getCurrentScore model)
            GetScoreAnswer (Err e) -> handleErr e
            GetScoreAnswer (Ok x) -> ({model | currentScore = x }, Cmd.none)
            BoardResponse (Err e) -> handleErr e
            BoardResponse (Ok b) -> ( { model | board = b }, Cmd.none)
            GetMovesResponse (Err e) -> handleErr e
            GetMovesResponse (Ok locs) -> ( { model | board = highlight model.board locs, possibleMoves = Just locs }, Cmd.none)
            MakeMoveResponse (Err e) -> handleErr e
            MakeMoveResponse (Ok b) -> ( { model | board = b, possibleMoves = Nothing }, Cmd.none)

handleClick : Model -> Location -> Cmd Msg
handleClick model location = case model.possibleMoves of
    Nothing -> getPossibleMoves model.board location
    Just _ -> makeMove model.board location

makeMove : Board -> Location -> Cmd Msg
makeMove board location =
    let
        jsonBoard = board |> boardToJsonValue |> Encode.encode 0
        jsonLocation = location |> locationToJsonValue |> Encode.encode 0
        uri = "http://localhost:5000/makeMove?board=" ++ jsonBoard ++ "&location=" ++ jsonLocation

        request = Http.get uri boardDecoder
    in
        Http.send MakeMoveResponse request
        

highlight : Board -> List Location -> Board
highlight board = List.foldl (\lc board -> Matrix.update lc (\_ -> Highlighted) board) board

getPossibleMoves : Board -> Location -> Cmd Msg
getPossibleMoves board location =
    let
        jsonBoard = board |> boardToJsonValue |> Encode.encode 0
        jsonLocation = location |> locationToJsonValue |> Encode.encode 0
        url = "http://localhost:5000/getMoves?board=" ++ jsonBoard ++ "&location=" ++ jsonLocation

        locationFromList : List Int -> Location
        locationFromList lst = case lst of
            [x, y] -> Matrix.loc x y
            _ -> Matrix.loc 0 0
        decodeLocation : Decode.Decoder Location
        decodeLocation = Decode.list Decode.int |> Decode.andThen (\lst -> lst |> locationFromList |> Decode.succeed)

        listOfMovesDecoder : Decode.Decoder (List Location)
        listOfMovesDecoder = Decode.list decodeLocation
        request = Http.get url listOfMovesDecoder
    in
        Http.send GetMovesResponse request

locationToJsonValue : Location -> Encode.Value
locationToJsonValue location = 
    let
        x = Matrix.row location
        y = Matrix.col location
    in
        Encode.list [Encode.int x, Encode.int y]

boardListDecoder : Decode.Decoder (List Board)
boardListDecoder =
    let
        decodeField : Decode.Decoder Field
        decodeField =
            Decode.int
                |> Decode.andThen (\x ->
                case x of
                        0 -> Decode.succeed Empty
                        1 -> Decode.succeed Black
                        2 -> Decode.succeed White
                        3 -> Decode.succeed King
                        rest -> Decode.fail <| "Unknown theme: " ++ toString rest
                )
        decodeRow : Decode.Decoder (List Field)
        decodeRow = Decode.list decodeField
        decode2dArr : Decode.Decoder (List (List Field))
        decode2dArr = Decode.list decodeRow
        decodeBoard : Decode.Decoder Board
        decodeBoard = decode2dArr |> Decode.andThen (\l -> Matrix.fromList l |> Decode.succeed)
    in
        Decode.list decodeBoard

loadHistoryFromTextArea : String -> List Board
loadHistoryFromTextArea v =
    let
        decodedVal = Decode.decodeString boardListDecoder v
    in
        decodedVal |> Result.toMaybe |> withDefault []

getCurrentScore : Model -> Cmd Msg
getCurrentScore model =
    let
        jsonVal = boardToJsonValue model.board
        url = "http://localhost:5000/getScore?board=" ++ (Encode.encode 0 jsonVal)
        getScoreDecoder : Decode.Decoder String
        getScoreDecoder = Decode.string
        request = Http.get url getScoreDecoder
    in
        Http.send GetScoreAnswer request

boardToJsonValue : Board -> Encode.Value
boardToJsonValue b =
    let
        fieldToChar field =
            case field of
                Empty -> '.'
                Highlighted -> '.'
                White -> 'd'
                Black -> 'a'
                King -> 'k'
        toRows : Board -> List String
        toRows b = b |> Matrix.toList |> List.map (\inner -> inner |> List.map fieldToChar |> String.fromList)
    in
        b |> toRows |> List.map Encode.string |> Encode.list

getBoard : Cmd Msg
getBoard =
  let
    url = "http://localhost:5000/getBoard"

    request =
      Http.get url boardDecoder
  in
    Http.send BoardResponse request

boardDecoder : Decode.Decoder Board
boardDecoder =
    let
        charToField : Char -> Field
        charToField c = case c of
            '.' -> Empty
            'a' -> Black
            'd' -> White
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

getHistory : Cmd Msg
getHistory =
    let
        url = "http://www.mocky.io/v2/5a70da04330000a550ff5e43"
        request = Http.get url boardListDecoder
    in
        Http.send NewHistory request


-- VIEW


view : Model -> Html Msg
view model =
    div [ style [("display", "flex"), ("flex-direction", "row")] ] [
        div [] (
            model.board
                |> Matrix.mapWithLocation (\lc e -> div [ myStyle (pickColor e), onClick (Clicked lc) ] [])
                |> Matrix.toList
                |> List.map (div [])
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
            , div [ errStyle ] [ text model.errorText ]
            , Html.button [ onClick GetBoardClicked ] [ text "Get board" ]
            , Html.button [ onClick GetScore ] [ text ("Get score") ]
            , Html.text model.currentScore
        ]
    ]

pickColor : Field -> String
pickColor f = case f of
    Empty       -> "peru"
    White       -> "white"
    Black       -> "black"
    Highlighted -> "orange"
    King        -> "purple"

errStyle : Attribute Msg
errStyle = style [
      ("height", "100px")
    , ("width", "100px")
    ]

myStyle : String -> Attribute Msg
myStyle clr =
  style
    [ ("backgroundColor", clr)
    , ("height", "50px")
    , ("width", "50px")
    , ("border", "2px black solid")
    , ("display", "inline-block")
    ]
