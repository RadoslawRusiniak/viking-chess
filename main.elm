import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model = Int


model : Model
model =
  0



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW


view : Model -> Html Msg
view _ = grid 11

cell : Html Msg
cell = div [ myStyle ] [ ]

row : Int -> Html Msg
row size = div [ ] (List.repeat size cell)

grid : Int -> Html Msg
grid size = div [] (List.repeat size (row size))

myStyle : Attribute Msg
myStyle =
  style
    [ ("backgroundColor", "yellow")
    , ("height", "50px")
    , ("width", "50px")
    , ("border", "2px black solid")
    , ("display", "inline-block")
    ]
