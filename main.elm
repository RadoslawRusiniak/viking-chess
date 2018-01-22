import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (square, toList)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type Field = Empty | White | Black

type alias Board = Matrix.Matrix Field
type alias Model = Board


model : Model
model = Matrix.square 11 (\_ -> Empty)



-- UPDATE


type Msg = Clicked Field


update : Msg -> Model -> Model
update msg model =
  case msg of
    Clicked _ -> Matrix.map (\x -> White) model



-- VIEW


view : Model -> Html Msg
view model = model 
    |> Matrix.toList 
    |> List.map (List.map (\e -> div [ myStyle (pickColor e), onClick (Clicked e) ] [ ]))
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
