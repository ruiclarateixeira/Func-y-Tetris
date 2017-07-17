-- Imports


module Main exposing (..)

import Html exposing (..)
import Board exposing (..)
import Time exposing (..)
import Keyboard exposing (..)
import Char exposing (..)
import Random exposing (..)


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
    | NewPiece Int


updateOnTimeTick : Model -> ( Model, Cmd Msg )
updateOnTimeTick board =
    let
        currentPiece =
            Maybe.withDefault (initPiece None) board.currentPiece
    in
        case currentPiece.pieceType of
            None ->
                ( board, Random.generate NewPiece (Random.int 0 4) )

            _ ->
                ( movePiece board Down, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flip ->
            ( model, Cmd.none )

        TimeTick time ->
            updateOnTimeTick model

        Presses code ->
            ( slide model code, Cmd.none )

        NewPiece n ->
            case n of
                0 ->
                    ( newPiece model (initPiece LShape), Cmd.none )

                1 ->
                    ( newPiece model (initPiece TShape), Cmd.none )

                2 ->
                    ( newPiece model (initPiece IShape), Cmd.none )

                3 ->
                    ( newPiece model (initPiece SShape), Cmd.none )

                4 ->
                    ( newPiece model (initPiece OShape), Cmd.none )

                _ ->
                    ( newPiece model (initPiece LShape), Cmd.none )



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
