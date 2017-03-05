module Board exposing (..)

import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Cell =
    { color : Maybe String }


type alias Row =
    { cells : List Cell }


type alias Board =
    { rows : List Row }



-- Board Functions


initBoard : Int -> Int -> Board
initBoard height width =
    { rows =
        repeat height
            ({ cells = repeat width { color = (Just "black") } })
    }


newPiece : Board -> Piece -> Board
newPiece board piece =
    let
        increment : ( Int, Int ) -> ( Int, Int )
        increment ( x, y ) =
            ( x, y + (length board.rows) )

        position : List ( Int, Int )
        position =
            List.map increment piece.coordinates
    in
        board



-- Piece Functions


type PieceType
    = LShape


type alias Piece =
    { pieceType : PieceType
    , color : String
    , coordinates : List ( Int, Int )
    }


initPiece : PieceType -> String -> Piece
initPiece pieceType color =
    case pieceType of
        LShape ->
            { pieceType = LShape
            , color = color
            , coordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
            }



-- Render Functions


renderBoard : Board -> Html msg
renderBoard board =
    div
        [ class "Board" ]
        (List.map renderRow board.rows)


renderRow : Row -> Html msg
renderRow row =
    div
        [ class "Row"
        , style [ ( "clear", "both" ) ]
        ]
        (List.map renderCell row.cells)


renderCell : Cell -> Html msg
renderCell cell =
    div
        [ class "Cell"
        , style [ ( "float", "left" ) ]
        ]
        [ text (Maybe.withDefault "none" cell.color) ]
