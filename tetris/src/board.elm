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
        firstRowCells =
            (Maybe.withDefault { cells = [] } (List.head board.rows)).cells

        offset rowSize =
            if (rem rowSize 2) == 0 then
                1
            else
                0

        increment : ( Int, Int ) -> ( Int, Int )
        increment ( x, y ) =
            ( x + (length firstRowCells) // 2 - (offset (length firstRowCells)), y + (length board.rows - 1) )

        position : List ( Int, Int )
        position =
            List.map increment piece.coordinates

        newPiece =
            { pieceType = piece.pieceType, color = piece.color, coordinates = position }
    in
        placePiece board newPiece


placePiece : Board -> Piece -> Board
placePiece board piece =
    let
        updateCell : ( Int, Int ) -> Maybe Cell
        updateCell position =
            if (List.member position piece.coordinates) then
                (Just { color = (Just piece.color) })
            else
                Nothing

        updateCells : Int -> Row -> Row
        updateCells rowIndex row =
            { cells =
                List.indexedMap
                    (\cellIndex cell ->
                        Maybe.withDefault cell (updateCell ( cellIndex, rowIndex ))
                    )
                    row.cells
            }
    in
        { rows = List.indexedMap updateCells board.rows }



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
