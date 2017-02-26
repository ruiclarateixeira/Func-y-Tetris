module Board exposing (initBoard)

import Array exposing (..)


type alias Row =
    { cells : Array String }


type alias Board =
    { rows : Array Row }


initBoard : Int -> Int -> Board
initBoard height width =
    { rows =
        initialize height
            (always
                ({ cells = initialize width (always "") })
            )
    }
