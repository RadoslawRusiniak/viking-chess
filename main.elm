import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, square, toList, Location, row, col)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type Field = Empty | White | Black

type alias Board = Matrix (Field, Location)
type alias Model = 
    { board : Board
    , selected : Maybe Location
    }


model : Model
model = Model
    (square 11 (\location -> (if row location == 0 then Black else Empty, location)))
    (Nothing)


-- UPDATE


type Msg = Clicked Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked lc -> { model | board = Matrix.update lc (\_ -> (White, lc)) model.board }



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
    Empty -> "yellow"
    White -> "white"
    Black -> "black"

myStyle : String -> Attribute Msg
myStyle clr =
  style
    [ ("backgroundColor", clr)
    , ("height", "50px")
    , ("width", "50px")
    , ("border", "2px black solid")
    , ("display", "inline-block")
    ]
