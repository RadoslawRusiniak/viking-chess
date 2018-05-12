module View exposing (view)

import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix
import Model exposing (Model, Mode(..), Pawn(..), Msg)


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


type alias OnFinishEdit =
    Msg


view : OnFieldClicked -> OnGetHint -> OnGetScore -> OnPrev -> OnNext -> OnSideChange -> OnEdit -> OnFinishEdit -> Model -> Html Msg
view onFieldClicked onGetHint onGetScore onPrev onNext onSideChange onEdit onFinishEdit model =
    div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
        [ displayBoardWithPawns onFieldClicked model
        , displayOptionsPanel onGetHint onGetScore onPrev onNext onSideChange onEdit onFinishEdit model
        ]


displayOptionsPanel : OnGetHint -> OnGetScore -> OnPrev -> OnNext -> OnSideChange -> OnEdit -> OnFinishEdit -> Model -> Html Msg
displayOptionsPanel onGetHint onGetScore onPrev onNext onSideChange onEdit onFinishEdit model =
    let
        isEditing =
            model.mode == Edit

        isPlaying =
            model.mode == Game
    in
        div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "justify-content", "space-around" ) ] ]
            [ Html.text ("Now moves: " ++ toString (Tuple.second model.state))
            , Html.button [ onClick onSideChange ] [ text ("Change side") ]
            , Html.button [ onClick onGetHint ] [ text ("Get hint") ]
            , Html.button [ onClick onPrev, Html.Attributes.disabled (List.isEmpty model.historyPrev) ] [ text "prev" ]
            , Html.button [ onClick onNext, Html.Attributes.disabled (List.isEmpty model.historyNext) ] [ text "next" ]
            , Html.button [ onClick onGetScore ] [ text ("Get score") ]
            , Html.text (toString model.currentScore)
            , Html.button [ onClick onEdit, Html.Attributes.disabled isEditing ] [ text ("Edit") ]
            , Html.button [ onClick onFinishEdit, Html.Attributes.disabled isPlaying ] [ text ("Finish edit") ]
            , div [ errStyle ] [ text model.errorText ]
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
            Maybe.map (\elem -> div [ pawnStyle elem ] []) >> Maybe.withDefault (div [] [])
    in
        div []
            (model.state
                |> Tuple.first
                |> Matrix.mapWithLocation (\loc elem -> div [ getFieldStyle loc, onClick (onFieldClicked loc) ] [ setPawn elem ])
                |> Matrix.toList
                |> List.map (div [ style [ ( "height", "56px" ) ] ])
            )


errStyle : Attribute Msg
errStyle =
    style
        [ ( "height", "100px" )
        , ( "width", "100px" )
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


fieldStyle : IsHighlighted -> IsClicked -> IsPartOfHint -> IsCorner -> IsThrone -> Attribute Msg
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
        style
            [ ( "backgroundColor", background )
            , ( "height", "50px" )
            , ( "width", "50px" )
            , ( "border", "3px solid black" )
            , ( "display", "inline-block" )
            ]


pawnStyle : Pawn -> Attribute Msg
pawnStyle pawn =
    let
        representationColor pawn =
            case pawn of
                Defender ->
                    "white"

                Attacker ->
                    "grey"

                King ->
                    "gold"
    in
        style
            [ ( "backgroundColor", representationColor pawn )
            , ( "height", "44px" )
            , ( "width", "44px" )
            , ( "margin-left", "3px" )
            , ( "margin-top", "3px" )
            , ( "display", "inline-block" )
            , ( "position", "absolute" )
            , ( "-webkit-border-radius", "44px" )
            ]
