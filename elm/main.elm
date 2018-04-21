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
    , selected : Maybe Location
    , historyPrev : List Board
    , historyNext : List Board
    , textareavalue : String
    , errorText : String
    , whoStartsVal : String
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
        "0"
        ""
    , getPawnsPositions)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- UPDATE

type alias HttpRes a = Result Http.Error a

type Msg = Clicked Location | PawnsPositions (HttpRes (List Location, List Location))
    | LoadHistoryViaHttp
    | NewHistory (HttpRes (List Board))
    | UpdateTextAreaValue String
    | LoadHistoryFromTextArea
    | Next | Prev
    | WhoStarts | WhoStartsAnswer (HttpRes String)
    | GetScore | GetScoreAnswer (HttpRes String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        handleErr e = ({ model | errorText = toString e}, Cmd.none)
        emptyBoard = Matrix.square 0 (\_ -> Empty)
        insertHistoryIntoModel data = ({model | historyPrev = [], board = List.head data |> withDefault emptyBoard, historyNext = List.tail data |> withDefault []}, Cmd.none)
    in
        case msg of
            Clicked lc -> (model, Cmd.none)
            PawnsPositions (Ok (lstW, lstB)) -> ( { model | board = model.board |> placePawns lstW White |> placePawns lstB Black }, Cmd.none)
            PawnsPositions (Err e) -> handleErr e
            LoadHistoryViaHttp -> (model, getHistory)
            NewHistory (Ok h) -> insertHistoryIntoModel h
            NewHistory (Err e) -> handleErr e
            UpdateTextAreaValue v -> ( { model | textareavalue = v}, Cmd.none)
            LoadHistoryFromTextArea -> loadHistoryFromTextArea model.textareavalue |> insertHistoryIntoModel
            Next -> ({model | historyPrev = model.board :: model.historyPrev, board = List.head model.historyNext |> withDefault emptyBoard, historyNext = List.tail model.historyNext |> withDefault []}, Cmd.none)
            Prev -> ({model | historyPrev = List.tail model.historyPrev |> withDefault [], board = List.head model.historyPrev |> withDefault emptyBoard, historyNext = model.board :: model.historyNext}, Cmd.none)
            WhoStarts -> (model, getWhoStarts)
            WhoStartsAnswer (Err e) -> handleErr e
            WhoStartsAnswer (Ok x) -> ({model | whoStartsVal = x }, Cmd.none)
            GetScore -> (model, getCurrentScore model)
            GetScoreAnswer (Err e) -> handleErr e
            GetScoreAnswer (Ok x) -> ({model | currentScore = x }, Cmd.none)

placePawns : List Location -> Field -> Board -> Board
placePawns lst f brd = lst |> List.foldl (\lc b -> Matrix.set lc f b) brd

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

getWhoStarts : Cmd Msg
getWhoStarts = 
    let
        url = "http://localhost:5000/whoStarts"
        whoStartsDecoder : Decode.Decoder String
        whoStartsDecoder = Decode.string
        request = Http.get url whoStartsDecoder
    in
        Http.send WhoStartsAnswer request

getCurrentScore : Model -> Cmd Msg
getCurrentScore model =
    let
        board = encodeBoard model.board
        url = "http://localhost:5000/getScore?board=" ++ (Encode.encode 0 board)
        getScoreDecoder : Decode.Decoder String
        getScoreDecoder = Decode.string
        request = Http.get url getScoreDecoder
    in
        Http.send GetScoreAnswer request

encodeBoard : Board -> Encode.Value
encodeBoard b =
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
        

getHistory : Cmd Msg
getHistory =
    let
        url = "http://www.mocky.io/v2/5a70da04330000a550ff5e43"
        request = Http.get url boardListDecoder
    in
        Http.send NewHistory request

getPawnsPositions : Cmd Msg
getPawnsPositions = 
  let
    url =
      "http://www.mocky.io/v2/5a6e0f622e00007f1eb8db4d"

    request =
      Http.get url decoder

    decoder : Decode.Decoder (List Location, List Location)
    decoder =
        let
            decodeLocation : Decode.Decoder Location
            decodeLocation = Decode.map2 (,) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)
            decodeLocsList : Decode.Decoder (List Location)
            decodeLocsList = Decode.list decodeLocation
            decodePairOfLists : Decode.Decoder (List Location, List Location)
            decodePairOfLists = Decode.map2 (,) (Decode.field "white" decodeLocsList) (Decode.field "black" decodeLocsList)
        in
            decodePairOfLists
  in
    Http.send PawnsPositions request

handleClick : Location -> Model -> Model
handleClick lc model =
    let
        getVal lc mx = Matrix.get lc mx |> withDefault Empty
        isEmpty lc mx = getVal lc mx == Empty || getVal lc mx == Highlighted
    in
        case (model.selected, isEmpty lc model.board) of
            (Nothing, True)     -> model
            (Nothing, False)    -> { model | selected = Just lc, board = manageHighlight (Just lc) model.board }
            (Just slc, True)    -> { model | board = swapCells lc slc model.board |> manageHighlight Nothing, selected = Nothing }
            (Just slc, False)   -> { model | selected = Just lc, board = manageHighlight (Just lc) model.board} 

manageHighlight : Maybe Location -> Board -> Board
manageHighlight mnewlc b =
    let
        removeHighlight = Matrix.map (\e -> if e == Highlighted then Empty else e)
        highlight lc b = 
            let
                okRow r = (r == Matrix.row lc)
                okCol c = (c == Matrix.col lc)
                toBeHighlighted (r,c) e = (okRow r || okCol c) && e == Empty
            in
                Matrix.mapWithLocation (\lc e -> if toBeHighlighted lc e then Highlighted else e) b
    in
        case mnewlc of
            Nothing -> b |> removeHighlight
            Just newlc -> b |> removeHighlight |> highlight newlc

swapCells : Location -> Location -> Board -> Board
swapCells lc1 lc2 b = 
    let
        val1 = Matrix.get lc1 b |> withDefault White
        val2 = Matrix.get lc2 b |> withDefault White
    in
        b |> Matrix.set lc1 val2 |> Matrix.set lc2 val1


-- VIEW


view : Model -> Html Msg
view model = 
    div [] [
        div [] (
            model.board
                |> Matrix.mapWithLocation (\lc e -> div [ myStyle (pickColor e), onClick (Clicked lc) ] [])
                |> Matrix.toList
                |> List.map (div [])
        )
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
        , div [ errStyle ] [ text model.errorText ]
        , Html.button [ onClick WhoStarts ] [ text ("who starts? " ++ model.whoStartsVal) ]
        , Html.button [ onClick GetScore ] [ text ("Get score") ]
        , Html.text model.currentScore
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
