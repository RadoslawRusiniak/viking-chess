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

type alias Board = Matrix (Field, Location)
type alias Model = 
    { board : Board
    , selected : Maybe Location
    }

model : Model
model = Model
    (square 11 (\location -> (if row location == 0 then Black else Empty, location)))
    Nothing


-- UPDATE


type Msg = Clicked Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked lc -> handleClick lc model

handleClick : Location -> Model -> Model
handleClick lc model = 
    case model.selected of
        Nothing -> { model | selected = Just lc, board = highlight lc model.board }
        Just slc -> { model | board = swapCells lc slc model.board, selected = Nothing }

highlight : Location -> Board -> Board
highlight lc b = 
    let
        okRow r = (r == Matrix.row lc)
        okCol c = (c == Matrix.col lc)
        toBeHighlighted r c e = (okRow r || okCol c) && e == Empty
    in
        Matrix.mapWithLocation (\(r, c) (e, olc) -> if toBeHighlighted r c e then (Highlighted, olc) else (e, olc)) b

swapCells : Location -> Location -> Board -> Board
swapCells lc1 lc2 b = 
    let
        val1 = Matrix.get lc1 b |> Maybe.map Tuple.first |> withDefault White
        val2 = Matrix.get lc2 b |> Maybe.map Tuple.first |> withDefault White
    in
        b |> Matrix.set lc1 (val2, lc1) |> Matrix.set lc2 (val1, lc2)
        |> Matrix.map (\(e, lc) -> if e == Highlighted then (Empty, lc) else (e, lc))


-- VIEW


view : Model -> Html Msg
view model =
    model.board
    |> Matrix.toList 
    |> List.map (List.map (\(f, lc) -> div [ myStyle (pickColor f), onClick (Clicked lc) ] [ ]))
    |> List.map (\l -> div [] l)
    |> div []

pickColor : Field -> String
pickColor f = case f of
    Empty       -> "yellow"
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
