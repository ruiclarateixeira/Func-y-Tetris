module Board exposing (..)

import List exposing (..)
import List.Extra exposing (..)
import Time exposing (..)
import MyUtils exposing (..)
import Pieces exposing (..)


type Msg
    = Flip
    | TimeTick Time
    | Presses Char
    | NewPiece Int
    | Reset


type Direction
    = Down
    | Left
    | Right


type alias Row =
    { cells : List PieceType }


type alias Board =
    { rows : List Row
    , currentPiece : Piece
    , lost : Bool
    , score : Int
    }



-- Board Functions


initBoard : Int -> Int -> Board
initBoard height width =
    { rows =
        repeat height
            ({ cells = repeat width None })
    , currentPiece = initPiece None
    , lost = False
    , score = 0
    }


newPiece : Board -> PieceType -> Board
newPiece board pieceType =
    let
        piece =
            initPiece pieceType

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

        position =
            increment piece.position

        newPiece =
            { piece | position = position }
    in
        { board | currentPiece = newPiece }



-- Adds piece to the board in the correct coordinates


placePiece : Board -> Piece -> Bool -> Board
placePiece board piece checkIfLost =
    let
        pieceCoordinates =
            getPieceCoordinates piece

        coordinatesOutsideBoard =
            filter (\( x, y ) -> y > (length board.rows)) pieceCoordinates

        lost =
            if length coordinatesOutsideBoard > 0 then
                True && checkIfLost
            else
                False

        updateCell : ( Int, Int ) -> Maybe PieceType
        updateCell position =
            if (List.member position pieceCoordinates) then
                (Just piece.pieceType)
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
        { board | rows = List.indexedMap updateCells board.rows, lost = lost }



-- Project board to be displayed
-- Places the current piece in the correct cells


projectBoard : Board -> Board
projectBoard board =
    placePiece board board.currentPiece False


pieceWillEnterCell : Board -> Piece -> Bool
pieceWillEnterCell board piece =
    let
        pieceCoordinates =
            getPieceCoordinates piece

        cellsInLastRow =
            filter (\( x, y ) -> y < 0) pieceCoordinates

        row : Int -> List PieceType
        row rowIndex =
            (Maybe.withDefault { cells = [] } (getAt rowIndex board.rows)).cells

        cellsWhichWillEnterAFilledCell =
            filter (\( x, y ) -> (Maybe.withDefault None (getAt x (row y)) /= None)) pieceCoordinates
    in
        length cellsWhichWillEnterAFilledCell /= 0



-- Can the piece move to the sides
-- 1. Going outside of the x range
-- 2. Going into a new cell


canMoveSides : Board -> Piece -> Bool
canMoveSides board piece =
    let
        pieceCoordinates =
            getPieceCoordinates piece

        cellsInFirstOrLastColumn =
            let
                row =
                    Maybe.withDefault { cells = [] } (getAt 0 board.rows)
            in
                filter (\( x, y ) -> x < 0 || x >= (length row.cells)) pieceCoordinates
    in
        length cellsInFirstOrLastColumn == 0 && not (pieceWillEnterCell board piece)



-- Can the piece move down
-- 1. Going further down than the min y
-- 2. Entering other filled cells


canMoveDown : Board -> Piece -> Bool
canMoveDown board piece =
    let
        pieceCoordinates =
            getPieceCoordinates piece

        cellsInLastRow =
            filter (\( x, y ) -> y < 0) pieceCoordinates
    in
        length cellsInLastRow == 0 && not (pieceWillEnterCell board piece)



-- Filter Filled Rows
-- Remove filled rows and add new ones at the top


filterFilledRows : List Row -> ( List Row, Int )
filterFilledRows rows =
    let
        filledCellFilter cell =
            if cell == None then
                Just cell
            else
                Nothing

        filledRowFilter row =
            let
                emptyCells =
                    filterMap filledCellFilter row.cells
            in
                if length emptyCells == 0 then
                    Nothing
                else
                    Just row

        newRows =
            filterMap filledRowFilter rows

        rowDiff =
            length rows - length newRows

        rowSize =
            length (Maybe.withDefault { cells = [] } (getAt 0 rows)).cells

        rowsToAdd =
            repeat rowDiff ({ cells = (repeat rowSize None) })
    in
        ( append newRows rowsToAdd, rowDiff )



-- Post Move Checks
-- 1. Remove complete lines and increment score


postMoveChecks : Board -> Board
postMoveChecks board =
    let
        ( newRows, rowDiff ) =
            filterFilledRows board.rows
    in
        { board | rows = newRows, score = board.score + rowDiff }



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

        ( x, y ) =
            board.currentPiece.position

        proposedPiece =
            { pieceType = board.currentPiece.pieceType
            , position = ( x + xOffset, y + yOffset )
            , baseCoordinates = board.currentPiece.baseCoordinates
            }

        ( newBoard, newPiece ) =
            if direction == Down && (canMoveDown board proposedPiece) then
                ( board, proposedPiece )
            else if (direction == Left || direction == Right) && (canMoveSides board proposedPiece) then
                ( board, proposedPiece )
            else if (direction == Left || direction == Right) then
                ( board, board.currentPiece )
            else
                ( placePiece board board.currentPiece True, initPiece None )
    in
        postMoveChecks { newBoard | currentPiece = newPiece, score = board.score }



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


rotatePiece : Board -> Board
rotatePiece board =
    let
        matrix =
            getPieceMatrix board.currentPiece

        rotated =
            rotateSquareMatrix matrix

        newPiece =
            { pieceType = board.currentPiece.pieceType
            , baseCoordinates = getPieceCoordinatesFromMatrix rotated
            , position = board.currentPiece.position
            }
    in
        if (canMoveDown board newPiece) && (canMoveSides board newPiece) then
            { board | currentPiece = newPiece }
        else
            board
