module Board exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Row =
    { cells : Array Int }


type alias Board =
    { rows : Array Row }


initBoard : Int -> Int -> Board
initBoard height width =
    { rows =
        initialize height
            (always
                ({ cells = initialize width (always 0) })
            )
    }



-- Render Functions


renderBoard : Board -> Html msg
renderBoard board =
    div
        [ class "Board" ]
        (toList (Array.map renderRow board.rows))


renderRow : Row -> Html msg
renderRow row =
    div
        [ class "Row"
        , style [ ( "clear", "both" ) ]
        ]
        (toList (Array.map renderCell row.cells))


renderCell : Int -> Html msg
renderCell cell =
    div
        [ class "Cell"
        , style [ ( "float", "left" ) ]
        ]
        [ text "Cell" ]
