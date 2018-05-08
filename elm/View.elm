module View exposing (view)

import Html exposing (Html, Attribute, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix
import Model exposing (Model, Msg, Field(..))


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


view : OnFieldClicked -> OnGetHint -> OnGetScore -> OnPrev -> OnNext -> Model -> Html Msg
view onFieldClicked onGetHint onGetScore onPrev onNext model =
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

        getFieldStyle loc =
            fieldStyle (isHighlighted loc) (isClicked loc) (isPartOfHint loc)

        --TODO maybe nicer way of handling empty field
        setPawn elem =
            if Model.isEmptyField elem then
                div [] []
            else
                div [ pawnStyle elem ] []
    in
        div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
            [ div []
                (model.state
                    |> Tuple.first
                    |> Matrix.mapWithLocation (\loc elem -> div [ getFieldStyle loc, onClick (onFieldClicked loc) ] [ setPawn elem ])
                    |> Matrix.toList
                    |> List.map (div [ style [ ( "height", "56px" ) ] ])
                )
            , div []
                [ Html.text ("Now moves: " ++ toString (Tuple.second model.state))
                , Html.br [] []
                , Html.button [ onClick onPrev, Html.Attributes.disabled (List.isEmpty model.historyPrev) ] [ text "prev" ]
                , Html.button [ onClick onNext, Html.Attributes.disabled (List.isEmpty model.historyNext) ] [ text "next" ]
                , Html.button [ onClick onGetScore ] [ text ("Get score") ]
                , Html.text (toString model.currentScore)
                , Html.button [ onClick onGetHint ] [ text ("Get hint") ]
                , div [ errStyle ] [ text model.errorText ]
                ]
            ]


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


fieldStyle : IsHighlighted -> IsClicked -> IsPartOfHint -> Attribute Msg
fieldStyle highlighted clicked hint =
    let
        background =
            if hint then
                "green"
            else if highlighted then
                "orange"
            else if clicked then
                "red"
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


pawnStyle : Field -> Attribute Msg
pawnStyle f =
    let
        representationColor f =
            case f of
                Empty ->
                    "peru"

                Defender ->
                    "white"

                Attacker ->
                    "grey"

                King ->
                    "purple"
    in
        style
            [ ( "backgroundColor", representationColor f )
            , ( "height", "44px" )
            , ( "width", "44px" )
            , ( "margin-left", "3px" )
            , ( "margin-top", "3px" )
            , ( "display", "inline-block" )
            , ( "position", "absolute" )
            , ( "-webkit-border-radius", "44px" )
            ]
