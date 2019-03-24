module Messages exposing (Msg(..))

import Http
import Matrix
import Model exposing (Token, GameState, Move)
import Browser
import Url


type alias HttpRes a =
    Result Http.Error a


type Msg
    = Dummy
    | InitGameResponse (HttpRes ( Token, GameState, Int ))
    | Clicked Matrix.Location
    | Next
    | Prev
    | GetScore
    | GetScoreResponse (HttpRes Float)
    | GetMovesResponse (HttpRes (List Matrix.Location))
    | MakeMoveResponse (HttpRes GameState)
    | GetHint
    | GetHintResponse (HttpRes Move)
    | ChangeSide
    | EditPosition
    | ClearPawns
    | FinishEditing
    | UpdateStateResponse (HttpRes ())
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
