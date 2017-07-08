module Board exposing (..)

import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Cell =
    { color : Maybe String }


type alias Row =
    { cells : List Cell }


type alias Board =
    { rows : List Row
    , currentPiece : Maybe Piece
    , lost : Bool
    }


type PieceType
    = LShape
    | None


type alias Piece =
    { pieceType : PieceType
    , color : String
    , coordinates : List ( Int, Int )
    }


type Direction
    = Down



-- Board Functions


initBoard : Int -> Int -> Board
initBoard height width =
    { rows =
        repeat height
            ({ cells = repeat width { color = (Just "black") } })
    , currentPiece = Nothing
    , lost = False
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
        { rows = board.rows, currentPiece = (Just newPiece), lost = board.lost }



-- Adds piece to the board in the correct coordinates


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
        { rows = List.indexedMap updateCells board.rows, currentPiece = board.currentPiece, lost = board.lost }



-- Project board to be displayed
-- Places the current piece in the correct cells


projectBoard : Board -> Board
projectBoard board =
    let
        piece =
            Maybe.withDefault (initPiece None "") board.currentPiece
    in
        placePiece board piece


canMove : Board -> Bool
canMove board =
    let
        currentPiece =
            Maybe.withDefault (initPiece None "") board.currentPiece

        piecesInLastRow =
            filter (\( x, y ) -> y == 0) currentPiece.coordinates
    in
        length piecesInLastRow == 0


movePiece : Board -> Direction -> Board
movePiece board direction =
    let
        ( xOffset, yOffset ) =
            case direction of
                Down ->
                    ( 0, -1 )

        currentPiece =
            Maybe.withDefault (initPiece None "") board.currentPiece

        newPiece =
            if canMove board then
                { color = currentPiece.color
                , pieceType = currentPiece.pieceType
                , coordinates = List.map (\( x, y ) -> ( x + xOffset, y + yOffset )) currentPiece.coordinates
                }
            else
                (initPiece None "")

        newBoard =
            if canMove board then
                board
            else
                placePiece board currentPiece
    in
        { rows = newBoard.rows, currentPiece = (Just newPiece), lost = newBoard.lost }



{- Piece Functions -}


initPiece : PieceType -> String -> Piece
initPiece pieceType color =
    case pieceType of
        LShape ->
            { pieceType = LShape
            , color = color
            , coordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
            }

        None ->
            { color = "none"
            , pieceType = None
            , coordinates = []
            }



{- Render Functions -}
-- render board as a sequence of rendered rows


renderBoard : Board -> Html msg
renderBoard board =
    div
        [ class "Board" ]
        (reverse (List.map renderRow board.rows))



-- Render row as a sequence of rendered cells


renderRow : Row -> Html msg
renderRow row =
    div
        [ class "Row"
        , style [ ( "clear", "both" ) ]
        ]
        (List.map renderCell row.cells)



-- Render cell as an HTML colored div


renderCell : Cell -> Html msg
renderCell cell =
    let
        backgroundColor =
            Maybe.withDefault "white" cell.color
    in
        div
            [ class "Cell"
            , style
                [ ( "float", "left" )
                , ( "height", "30px" )
                , ( "width", "30px" )
                , ( "background-color", backgroundColor )
                ]
            ]
            [ text "" ]
