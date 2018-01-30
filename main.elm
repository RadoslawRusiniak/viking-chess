import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, square, toList, Location, row, col)
import Maybe exposing (withDefault)

import Json.Decode as Decode
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
    , history : List Board
    , txt : String
    }

init : (Model, Cmd Msg)
init =
    (Model
        (square 11 (\_ -> Empty))
        Nothing
        []
        ""
    , getPawnsPositions)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- UPDATE

type alias HttpRes a = Result Http.Error a

type Msg = Clicked Location | PawnsPositions (HttpRes (List Location, List Location)) | HighlightedArrived (HttpRes Int)
    | LoadHistory | NewHistory (HttpRes (List Board))
    | Next


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        handleErr e = ({ model | txt = toString e}, Cmd.none)
        emptyBoard = Matrix.square 0 (\_ -> Empty)
    in
        case msg of
            Clicked lc -> (model, getHighlighted)
            PawnsPositions (Ok (lstW, lstB)) -> ( { model | board = model.board |> placePawns lstW White |> placePawns lstB Black }, Cmd.none)
            PawnsPositions (Err e) -> handleErr e
            HighlightedArrived (Ok x) -> ( { model | board = model.board |> Matrix.set (x, 2) Highlighted }, Cmd.none)
            HighlightedArrived (Err e) -> handleErr e
            LoadHistory -> (model, getHistory)
            NewHistory (Ok h) -> ( { model | board = List.head h |> withDefault emptyBoard, history = List.tail h |> withDefault [] } , Cmd.none)
            NewHistory (Err e) -> handleErr e
            Next ->
                case List.length model.history of
                    0 -> (model, Cmd.none)
                    _ -> ({model | board = List.head model.history |> withDefault emptyBoard, history = List.tail model.history |> withDefault []}, Cmd.none)

placePawns : List Location -> Field -> Board -> Board
placePawns lst f brd = lst |> List.foldl (\lc b -> Matrix.set lc f b) brd

boardDecoder : Decode.Decoder Board
boardDecoder =
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
        decodeBoard

getHistory : Cmd Msg
getHistory =
    let
        url = "http://www.mocky.io/v2/5a70da04330000a550ff5e43"
        request = Http.get url boardListDecoder
        boardListDecoder : Decode.Decoder (List Board)
        boardListDecoder = Decode.list boardDecoder
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
        
getHighlighted : Cmd Msg
getHighlighted =
  let
    url =
      "http://www.mocky.io/v2/5a6dde3e2e00001e16b8daf7a"

    request =
      Http.get url decodeHighlighted

    decodeHighlighted : Decode.Decoder Int
    decodeHighlighted =
        Decode.field "x" Decode.int
  in
    Http.send HighlightedArrived request

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
        , div [ errStyle ] [ text model.txt ]
        , Html.button [ onClick LoadHistory ] [ text "Load history" ]
        , Html.button [ onClick Next ] [ text "next" ]
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
