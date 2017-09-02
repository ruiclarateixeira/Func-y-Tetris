module PiecesTests exposing (..)

import Test exposing (..)
import Expect
import Pieces exposing (..)


all : Test
all =
    describe "Pieces test suite"
        [ describe "piece coordinates to matrix"
            [ test "og lshape matrix" <|
                \() ->
                    Expect.equal (getPieceMatrix (initPiece LShape))
                        [ [ LShape, LShape, None ]
                        , [ LShape, None, None ]
                        , [ LShape, None, None ]
                        ]
            ]
        , describe "matrix to piece coordinates"
            [ test "lshape rotated matrix" <|
                \() ->
                    let
                        og =
                            [ [ Just None, Just None, Just None ]
                            , [ Just LShape, Just None, Just None ]
                            , [ Just LShape, Just LShape, Just LShape ]
                            ]

                        ex =
                            [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
                    in
                        Expect.equal (getPieceCoordinatesFromMatrix og) ex
            ]
        ]
