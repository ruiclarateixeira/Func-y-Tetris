-- Imports


module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- Model


type alias Piece =
    { color : String
    , position : Int
    , shape : String
    }


drawShape : Piece -> Html msg
drawShape piece =
    div
        [ class "piece"
        , style
            [ ( "color", "green" )
            ]
        ]
        [ text "Piece" ]



-- VIEW


view : Html msg
view =
    body []
        [ drawShape { color = "blue", position = 1, shape = "L" }
        ]


main : Html msg
main =
    view
