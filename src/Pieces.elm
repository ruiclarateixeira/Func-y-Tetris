module Pieces exposing (..)

import List exposing (..)


type PieceType
    = LShape
    | TShape
    | IShape
    | SShape
    | OShape
    | None


type alias Piece =
    { pieceType : PieceType
    , baseCoordinates : List ( Int, Int )
    , position : ( Int, Int )
    }


initPiece : PieceType -> Piece
initPiece pieceType =
    case pieceType of
        LShape ->
            { pieceType = LShape
            , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
            , position = ( 0, 0 )
            }

        TShape ->
            { pieceType = TShape
            , baseCoordinates = [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
            , position = ( 0, 0 )
            }

        IShape ->
            { pieceType = IShape
            , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
            , position = ( 0, 0 )
            }

        SShape ->
            { pieceType = SShape
            , baseCoordinates = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
            , position = ( 0, 0 )
            }

        OShape ->
            { pieceType = OShape
            , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]
            , position = ( 0, 0 )
            }

        None ->
            { pieceType = None
            , baseCoordinates = []
            , position = ( 0, 0 )
            }


getPieceCoordinates : Piece -> List ( Int, Int )
getPieceCoordinates piece =
    let
        ( posX, posY ) =
            piece.position

        offset ( x, y ) =
            ( x + posX, y + posY )
    in
        map offset piece.baseCoordinates


getPieceMatrix : Piece -> List (List ( Int, Int ))
getPieceMatrix piece =
    [ [ ( 0, 0 ) ] ]
