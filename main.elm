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
model = Model
    (square 11 (\location -> if row location == 0 then Black else Empty))
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
        toBeHighlighted (r,c) e = (okRow r || okCol c) && e == Empty
    in
        Matrix.mapWithLocation (\lc e -> if toBeHighlighted lc e then Highlighted else e) b

swapCells : Location -> Location -> Board -> Board
swapCells lc1 lc2 b = 
    let
        val1 = Matrix.get lc1 b |> withDefault White
        val2 = Matrix.get lc2 b |> withDefault White
    in
        b |> Matrix.set lc1 val2 |> Matrix.set lc2 val1
        |> Matrix.map (\e -> if e == Highlighted then Empty else e)


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
