module Board exposing (..)

import List exposing (..)
import List.Extra exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type Cell
    = Filled
    | Empty


type PieceType
    = LShape
    | TShape
    | None


type Direction
    = Down
    | Left
    | Right


type alias Row =
    { cells : List Cell }


type alias Board =
    { rows : List Row
    , currentPiece : Maybe Piece
    , lost : Bool
    }


type alias Piece =
    { pieceType : PieceType
    , coordinates : List ( Int, Int )
    }



-- Board Functions


initBoard : Int -> Int -> Board
initBoard height width =
    { rows =
        repeat height
            ({ cells = repeat width Empty })
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
            { pieceType = piece.pieceType, coordinates = position }
    in
        { rows = board.rows, currentPiece = (Just newPiece), lost = board.lost }



-- Adds piece to the board in the correct coordinates


placePiece : Board -> Piece -> Board
placePiece board piece =
    let
        updateCell : ( Int, Int ) -> Maybe Cell
        updateCell position =
            if (List.member position piece.coordinates) then
                (Just Filled)
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
            Maybe.withDefault (initPiece None) board.currentPiece
    in
        placePiece board piece



-- 1. Going outside of the x range


canShift : Board -> Piece -> Bool
canShift board piece =
    let
        cellsInFirstOrLastColumn =
            let
                row =
                    Maybe.withDefault { cells = [] } (getAt 0 board.rows)
            in
                filter (\( x, y ) -> x < 0 || x >= (length row.cells)) piece.coordinates
    in
        length cellsInFirstOrLastColumn == 0



-- Can the piece move to a new place without:
-- 1. Going further down than the min y
-- 2. Entering other filled cells


canMove : Board -> Piece -> Bool
canMove board piece =
    let
        cellsInLastRow =
            filter (\( x, y ) -> y < 0) piece.coordinates

        row : Int -> List Cell
        row rowIndex =
            (Maybe.withDefault { cells = [] } (getAt rowIndex board.rows)).cells

        cellsWhichWillEnterAFilledCell =
            filter (\( x, y ) -> (Maybe.withDefault Empty (getAt x (row y)) /= Empty)) piece.coordinates
    in
        length cellsInLastRow == 0 && length cellsWhichWillEnterAFilledCell == 0



-- Move piece coordinates in a given direction


movePiece : Board -> Direction -> Board
movePiece board direction =
    let
        ( xOffset, yOffset ) =
            case direction of
                Down ->
                    ( 0, -1 )

                Left ->
                    ( -1, 0 )

                Right ->
                    ( 1, 0 )

        currentPiece =
            Maybe.withDefault (initPiece None) board.currentPiece

        proposedPiece =
            { pieceType = currentPiece.pieceType
            , coordinates = List.map (\( x, y ) -> ( x + xOffset, y + yOffset )) currentPiece.coordinates
            }

        ( newBoard, newPiece ) =
            if not (canShift board proposedPiece) then
                ( board, currentPiece )
            else if canMove board proposedPiece then
                ( board, proposedPiece )
            else
                ( placePiece board currentPiece, initPiece None )
    in
        { rows = newBoard.rows, currentPiece = (Just newPiece), lost = newBoard.lost }



-- Slide the piece manually given a key pressed


slide : Board -> Char -> Board
slide board code =
    let
        direction =
            case code of
                'a' ->
                    Left

                'd' ->
                    Right

                _ ->
                    Down
    in
        movePiece board direction



{- Piece Functions -}


initPiece : PieceType -> Piece
initPiece pieceType =
    case pieceType of
        LShape ->
            { pieceType = LShape
            , coordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
            }

        TShape ->
            { pieceType = TShape
            , coordinates = [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
            }

        None ->
            { pieceType = None
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
            case cell of
                Filled ->
                    "blue"

                Empty ->
                    "black"
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
