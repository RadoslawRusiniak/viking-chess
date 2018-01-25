import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, square, toList, Location, row, col)
import Maybe exposing (withDefault)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type Field = Empty | Highlighted | White | Black

type alias Board = Matrix Field
type alias Model = 
    { board : Board
    , selected : Maybe Location
    }

model : Model
model = 
    let
        isWhite lc = List.member lc [
                              (3, 5)
                    , (4, 4), (4, 5), (4, 6)     
            , (5, 3), (5, 4), (5, 5), (5, 6), (5, 7)
                    , (6, 4), (6, 5), (6, 6)
                            , (7, 5)
        ]

        isBlack lc = List.member lc [                   
                                (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)
                                              , (1, 5)
                                        
                                        
            , (3, 0)                                                               , (3, 10)
            , (4, 0)                                                               , (4, 10)
            , (5, 0), (5, 1)                                              , (5, 9) , (5, 10)
            , (6, 0)                                                               , (6, 10)
            , (7, 0)                                                               , (7, 10)
            
                                              , (9, 5)
                              , (10, 3), (10, 4), (10, 5), (10, 6), (10, 7)
        ]
    in
    Model
        (square 11 (\lc -> if isWhite lc then White else if isBlack lc then Black else Empty))
        Nothing


-- UPDATE


type Msg = Clicked Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked lc -> handleClick lc model

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
    model.board
    |> Matrix.mapWithLocation (\lc e -> div [ myStyle (pickColor e), onClick (Clicked lc) ] [])
    |> Matrix.toList
    |> List.map (\l -> div [] l)
    |> div []

pickColor : Field -> String
pickColor f = case f of
    Empty       -> "peru"
    White       -> "white"
    Black       -> "black"
    Highlighted -> "orange"

myStyle : String -> Attribute Msg
myStyle clr =
  style
    [ ("backgroundColor", clr)
    , ("height", "50px")
    , ("width", "50px")
    , ("border", "2px black solid")
    , ("display", "inline-block")
    ]
