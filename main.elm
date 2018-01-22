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


type alias Model = Bool


model : Model
model = False



-- UPDATE


type Msg = Boom


update : Msg -> Model -> Model
update msg model =
  case msg of
    Boom -> not model
        



-- VIEW


view : Model -> Html Msg
view model = div [] <| grid 11


cell : Html Msg
cell = div [ myStyle, onClick Boom ] [ ]

grid : Int -> List (Html Msg)
grid size = (square size (\_ -> cell)) |> toList |> List.map (\l -> div [] l)

myStyle : Attribute Msg
myStyle =
  style
    [ ("backgroundColor", "yellow")
    , ("height", "50px")
    , ("width", "50px")
    , ("border", "2px black solid")
    , ("display", "inline-block")
    ]
