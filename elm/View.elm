module View exposing (view)

import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix
import Messages exposing (Msg)
import Model exposing (Model, Mode(..), Pawn(..), GameState, WhoMoves)
import Browser


type alias OnFieldClicked =
    Matrix.Location -> Msg


type alias OnGetHint =
    Msg


type alias OnGetScore =
    Msg


type alias OnPrev =
    Msg


type alias OnNext =
    Msg


type alias OnSideChange =
    Msg


type alias OnEdit =
    Msg


type alias OnClearPawns =
    Msg


type alias OnFinishEdit =
    Msg


view : OnFieldClicked -> OnGetHint -> OnGetScore -> OnPrev -> OnNext -> OnSideChange -> OnEdit -> OnClearPawns -> OnFinishEdit -> Model -> Browser.Document Msg
view onFieldClicked onGetHint onGetScore onPrev onNext onSideChange onEdit onClearPawns onFinishEdit model =
    { title = "Strona1"
    , body =
        [ div
            [ style "display" "flex"
            , style "flex-direction" "row"
            ]
            [ displayBoardWithPawns onFieldClicked model
            , displayOptionsPanel onGetHint onGetScore onPrev onNext onSideChange onEdit onClearPawns onFinishEdit model
            ]
        ]
    }


displayOptionsPanel : OnGetHint -> OnGetScore -> OnPrev -> OnNext -> OnSideChange -> OnEdit -> OnClearPawns -> OnFinishEdit -> Model -> Html Msg
displayOptionsPanel onGetHint onGetScore onPrev onNext onSideChange onEdit onClearPawns onFinishEdit model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "width" "180px"
        , style "margin-left" "40px"
        , style "justify-content" "space-around"
        ]
        [ displayWhosTurnPanel onSideChange (Tuple.second model.state)
        , Html.button [ onClick onGetHint ] [ text ("Get hint") ]
        , displayHistoryPanel onPrev onNext model.historyPrev model.historyNext
        , displayScorePanel onGetScore model.currentScore
        , displayEditPanel onEdit onClearPawns onFinishEdit model.mode
        , div errStyle [ text model.errorText ]
        ]


displayWhosTurnPanel : OnSideChange -> WhoMoves -> Html Msg
displayWhosTurnPanel onSideChange whosTurn =
    div panelStyle
        [ Html.text ("Now moves: " ++ Model.toString whosTurn)
        , Html.button [ onClick onSideChange ] [ text ("Change side") ]
        ]


displayHistoryPanel : OnPrev -> OnNext -> List GameState -> List GameState -> Html Msg
displayHistoryPanel onPrev onNext historyPrev historyNext =
    div panelStyle
        [ Html.button [ onClick onPrev, Html.Attributes.disabled (List.isEmpty historyPrev) ] [ text "history prev" ]
        , Html.button [ onClick onNext, Html.Attributes.disabled (List.isEmpty historyNext) ] [ text "history next" ]
        ]


displayScorePanel : OnGetScore -> Float -> Html Msg
displayScorePanel onGetScore currentScore =
    div panelStyle
        [ Html.button [ onClick onGetScore ] [ text ("Get score") ]
        , Html.text (String.fromFloat currentScore)
        ]


displayEditPanel : OnEdit -> OnClearPawns -> OnFinishEdit -> Mode -> Html Msg
displayEditPanel onEdit onClearPawns onFinishEdit mode =
    div panelStyle
        [ Html.button [ onClick onEdit, Html.Attributes.disabled (mode == Edit) ] [ text ("Edit") ]
        , Html.button [ onClick onClearPawns, Html.Attributes.disabled (mode == Game) ] [ text ("Clear pawns") ]
        , Html.button [ onClick onFinishEdit, Html.Attributes.disabled (mode == Game) ] [ text ("Finish edit") ]
        ]


panelStyle : List (Attribute Msg)
panelStyle =
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "justify-content" "space-between"
    ]


displayBoardWithPawns : OnFieldClicked -> Model -> Html.Html Msg
displayBoardWithPawns onFieldClicked model =
    let
        isHighlighted lc =
            case model.possibleMoves of
                Nothing ->
                    False

                Just moves ->
                    List.member lc moves

        isClicked lc =
            case model.clickedLocation of
                Nothing ->
                    False

                Just clicked ->
                    lc == clicked

        isPartOfHint lc =
            case model.hint of
                Nothing ->
                    False

                Just ( from, to ) ->
                    lc == from || lc == to

        isCorner ( x, y ) =
            (x == 0 || x == model.boardSize - 1) && (y == 0 || y == model.boardSize - 1)

        isThrone ( x, y ) =
            x == (model.boardSize - 1) // 2 && y == (model.boardSize - 1) // 2

        getFieldStyle loc =
            fieldStyle (isHighlighted loc) (isClicked loc) (isPartOfHint loc) (isCorner loc) (isThrone loc)

        setPawn =
            Maybe.map (\elem -> div (pawnStyle elem) []) >> Maybe.withDefault (div [] [])
    in
        div []
            (model.state
                |> Tuple.first
                |> Matrix.mapWithLocation (\loc elem -> div (onClick (onFieldClicked loc) :: (getFieldStyle loc)) [ setPawn elem ])
                |> Matrix.toList
                |> List.map (div [ style "height" "56px" ])
            )


errStyle : List (Attribute Msg)
errStyle =
    [ style "height" "100px"
    , style "width" "100px"
    ]


type alias IsHighlighted =
    Bool


type alias IsClicked =
    Bool


type alias IsPartOfHint =
    Bool


type alias IsCorner =
    Bool


type alias IsThrone =
    Bool


fieldStyle : IsHighlighted -> IsClicked -> IsPartOfHint -> IsCorner -> IsThrone -> List (Attribute Msg)
fieldStyle highlighted clicked hint corner throne =
    let
        background =
            if hint then
                "green"
            else if highlighted then
                "orange"
            else if clicked then
                "red"
            else if corner then
                "pink"
            else if throne then
                "purple"
            else
                "peru"
    in
        [ style "backgroundColor" background
        , style "height" "50px"
        , style "width" "50px"
        , style "border" "3px solid black"
        , style "display" "inline-block"
        ]


pawnStyle : Pawn -> List (Attribute Msg)
pawnStyle pawn =
    let
        representationColor cpawn =
            case cpawn of
                Defender ->
                    "white"

                Attacker ->
                    "grey"

                King ->
                    "gold"
    in
        [ style "backgroundColor" (representationColor pawn)
        , style "height" "44px"
        , style "width" "44px"
        , style "margin-left" "3px"
        , style "margin-top" "3px"
        , style "display" "inline-block"
        , style "position" "absolute"
        , style "-webkit-border-radius" "44px"
        ]
