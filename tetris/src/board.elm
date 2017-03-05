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
        { rows = board.rows, currentPiece = (Just newPiece) }


projectBoard : Board -> Board
projectBoard board =
    let
        piece =
            Maybe.withDefault (initPiece None "") board.currentPiece

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
        { rows = List.indexedMap updateCells board.rows, currentPiece = (Just piece) }


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
            { color = currentPiece.color
            , pieceType = currentPiece.pieceType
            , coordinates = List.map (\( x, y ) -> ( x + xOffset, y + yOffset )) currentPiece.coordinates
            }
    in
        { rows = board.rows, currentPiece = (Just newPiece) }



-- Piece Functions


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



-- Render Functions


renderBoard : Board -> Html msg
renderBoard board =
    div
        [ class "Board" ]
        (reverse (List.map renderRow board.rows))


renderRow : Row -> Html msg
renderRow row =
    div
        [ class "Row"
        , style [ ( "clear", "both" ) ]
        ]
        (List.map renderCell row.cells)


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
