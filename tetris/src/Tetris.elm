-- Imports


module Main exposing (..)

import Html exposing (..)
import Board exposing (..)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    Board



-- Update


type Msg
    = Flip
    | TimeTick Time


updateOnTimeTick : Model -> Model
updateOnTimeTick board =
    let
        currentPiece =
            Maybe.withDefault (initPiece None) board.currentPiece
    in
        case currentPiece.pieceType of
            None ->
                newPiece board (initPiece LShape)

            _ ->
                movePiece board Down


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flip ->
            ( model, Cmd.none )

        TimeTick time ->
            ( updateOnTimeTick model, Cmd.none )



-- Init


init : ( Model, Cmd Msg )
init =
    ( initBoard 20 10, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    every second TimeTick



-- VIEW


view : Board -> Html Msg
view board =
    body []
        [ renderBoard (projectBoard board)
        ]
