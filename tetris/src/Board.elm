module Board exposing (..)

import List exposing (..)
import List.Extra exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type PieceType
    = LShape
    | TShape
    | IShape
    | SShape
    | OShape
    | None


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


type alias Piece =
    { pieceType : PieceType
    , baseCoordinates : List ( Int, Int )
    , position : List ( Int, Int )
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

        position : List ( Int, Int )
        position =
            List.map increment piece.position

        newPiece =
            { pieceType = piece.pieceType, position = position, baseCoordinates = piece.baseCoordinates }
    in
        { rows = board.rows, currentPiece = newPiece, lost = board.lost }



-- Adds piece to the board in the correct coordinates


placePiece : Board -> Piece -> Board
placePiece board piece =
    let
        updateCell : ( Int, Int ) -> Maybe PieceType
        updateCell position =
            if (List.member position piece.position) then
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



-- 1. Going outside of the x range


canShift : Board -> Piece -> Bool
canShift board piece =
    let
        cellsInFirstOrLastColumn =
            let
                row =
                    Maybe.withDefault { cells = [] } (getAt 0 board.rows)
            in
                filter (\( x, y ) -> x < 0 || x >= (length row.cells)) piece.position
    in
        length cellsInFirstOrLastColumn == 0



-- Can the piece move to a new place without:
-- 1. Going further down than the min y
-- 2. Entering other filled cells


canMove : Board -> Piece -> Bool
canMove board piece =
    let
        cellsInLastRow =
            filter (\( x, y ) -> y < 0) piece.position

        row : Int -> List PieceType
        row rowIndex =
            (Maybe.withDefault { cells = [] } (getAt rowIndex board.rows)).cells

        cellsWhichWillEnterAFilledCell =
            filter (\( x, y ) -> (Maybe.withDefault None (getAt x (row y)) /= None)) piece.position
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

        proposedPiece =
            { pieceType = board.currentPiece.pieceType
            , position = List.map (\( x, y ) -> ( x + xOffset, y + yOffset )) board.currentPiece.position
            , baseCoordinates = board.currentPiece.baseCoordinates
            }

        ( newBoard, newPiece ) =
            if not (canShift board proposedPiece) then
                ( board, board.currentPiece )
            else if canMove board proposedPiece then
                ( board, proposedPiece )
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
        rotate ( x, y ) =
            ( y, x )

        newPosition =
            List.map rotate board.currentPiece.position
    in
        { currentPiece = { pieceType = board.currentPiece.pieceType, position = newPosition, baseCoordinates = board.currentPiece.baseCoordinates }
        , rows = board.rows
        , lost = board.lost
        }


initPiece : PieceType -> Piece
initPiece pieceType =
    case pieceType of
        LShape ->
            { pieceType = LShape
            , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
            , position = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
            }

        TShape ->
            { pieceType = TShape
            , baseCoordinates = [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
            , position = [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
            }

        IShape ->
            { pieceType = IShape
            , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
            , position = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
            }

        SShape ->
            { pieceType = SShape
            , baseCoordinates = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
            , position = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
            }

        OShape ->
            { pieceType = OShape
            , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]
            , position = [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]
            }

        None ->
            { pieceType = None
            , baseCoordinates = []
            , position = []
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


renderCell : PieceType -> Html msg
renderCell pieceType =
    let
        backgroundColor =
            case pieceType of
                LShape ->
                    "blue"

                TShape ->
                    "red"

                IShape ->
                    "purple"

                SShape ->
                    "yellow"

                OShape ->
                    "green"

                None ->
                    "black"
    in
        div
            [ class "Cell"
            , style
                [ ( "float", "left" )
                , ( "height", "30px" )
                , ( "width", "30px" )
                , ( "margin", "0.5px" )
                , ( "background-color", backgroundColor )
                ]
            ]
            [ text "" ]
