module Board exposing (..)

import List exposing (..)
import List.Extra exposing (..)
import MyUtils exposing (..)
import Pieces exposing (..)


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
    }



-- Board Functions


initBoard : Int -> Int -> Board
initBoard height width =
    { rows =
        repeat height
            ({ cells = repeat width None })
    , currentPiece = initPiece None
    , lost = False
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
            { pieceType = piece.pieceType, position = position, baseCoordinates = piece.baseCoordinates }
    in
        { rows = board.rows, currentPiece = newPiece, lost = board.lost }



-- Adds piece to the board in the correct coordinates


placePiece : Board -> Piece -> Board
placePiece board piece =
    let
        pieceCoordinates =
            getPieceCoordinates piece

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
        { rows = List.indexedMap updateCells board.rows, currentPiece = board.currentPiece, lost = board.lost }



-- Project board to be displayed
-- Places the current piece in the correct cells


projectBoard : Board -> Board
projectBoard board =
    placePiece board board.currentPiece


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
                ( placePiece board board.currentPiece, initPiece None )
    in
        { rows = newBoard.rows, currentPiece = newPiece, lost = newBoard.lost }



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
            { rows = board.rows
            , currentPiece = newPiece
            , lost = board.lost
            }
        else
            board
