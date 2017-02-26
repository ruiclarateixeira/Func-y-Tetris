-- Imports


module Main exposing (..)

import Html exposing (..)
import Board exposing (..)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Flip ->
            ( model, Cmd.none )



-- Init


init : ( Model, Cmd Msg )
init =
    ( initBoard 10 5, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Board -> Html Msg
view board =
    body []
        [ renderBoard board
        ]
