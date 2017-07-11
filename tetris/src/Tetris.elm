-- Imports


module Main exposing (..)

import Html exposing (..)
import Board exposing (..)
import Time exposing (..)
import Keyboard exposing (..)
import Char exposing (..)


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
    | Presses Char


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

        Presses code ->
            ( slide model code, Cmd.none )



-- Init


init : ( Model, Cmd Msg )
init =
    ( initBoard 20 10, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every second TimeTick
        , Keyboard.presses (\code -> Presses (fromCode code))
        ]



-- VIEW


view : Board -> Html Msg
view board =
    body []
        [ renderBoard (projectBoard board)
        ]