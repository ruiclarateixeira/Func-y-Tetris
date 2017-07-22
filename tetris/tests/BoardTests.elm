module BoardTests exposing (..)

import Test exposing (..)
import List exposing (length, filter, repeat)
import Maybe exposing (withDefault)
import Expect
import Board exposing (..)


all : Test
all =
    describe "Board Test Suite"
        [ describe "Init board tests"
            [ test "Init board populates height" <|
                \() ->
                    let
                        board =
                            Board.initBoard 3 5
                    in
                        Expect.equal (length board.rows) 3
            , test "Init board populates width" <|
                \() ->
                    let
                        board =
                            Board.initBoard 3 5

                        lengthFilter =
                            \row -> (length row.cells) /= 5
                    in
                        Expect.equal (length (List.filter lengthFilter board.rows)) 0
            ]
        , describe "New Piece Tests"
            [ test "New piece works correctly" <|
                \() ->
                    let
                        board =
                            Board.initBoard 5 10

                        newBoard =
                            Board.newPiece board Board.LShape

                        boardWithPiece =
                            Board.projectBoard newBoard
                    in
                        Expect.equal boardWithPiece
                            { rows =
                                [ { cells = repeat 10 None }
                                , { cells = repeat 10 None }
                                , { cells = repeat 10 None }
                                , { cells = repeat 10 None }
                                , { cells =
                                        List.concat
                                            [ (repeat 4 None)
                                            , (repeat 2 LShape)
                                            , (repeat 4 None)
                                            ]
                                  }
                                ]
                            , currentPiece =
                                { pieceType = Board.LShape
                                , position = [ ( 4, 4 ), ( 4, 5 ), ( 4, 6 ), ( 5, 4 ) ]
                                , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
                                }
                            , lost = False
                            }
            ]
        , describe "Move Piece tests"
            [ test "Move Piece works correctly" <|
                \() ->
                    let
                        board =
                            Board.initBoard 5 10

                        newBoard =
                            Board.newPiece board Board.LShape

                        boardWithPiece =
                            Board.projectBoard (Board.movePiece newBoard Board.Down)
                    in
                        Expect.equal boardWithPiece
                            { rows =
                                [ { cells = repeat 10 None }
                                , { cells = repeat 10 None }
                                , { cells = repeat 10 None }
                                , { cells =
                                        List.concat
                                            [ (repeat 4 None)
                                            , (repeat 2 LShape)
                                            , (repeat 4 None)
                                            ]
                                  }
                                , { cells =
                                        List.concat
                                            [ (repeat 4 None)
                                            , (repeat 1 LShape)
                                            , (repeat 5 None)
                                            ]
                                  }
                                ]
                            , currentPiece =
                                { pieceType = Board.LShape
                                , position = [ ( 4, 3 ), ( 4, 4 ), ( 4, 5 ), ( 5, 3 ) ]
                                , baseCoordinates = [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]
                                }
                            , lost = False
                            }
            ]
        ]
