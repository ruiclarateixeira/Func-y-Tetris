module BoardTests exposing (..)

import Test exposing (..)
import List exposing (length, filter, repeat)
import Maybe exposing (withDefault)
import Expect
import Board


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

                        newPiece =
                            Board.initPiece Board.LShape "blue"

                        newBoard =
                            Board.newPiece board newPiece
                    in
                        Expect.equal board
                            { rows =
                                [ { cells = repeat 10 { color = (Just "black") } }
                                , { cells = repeat 10 { color = (Just "black") } }
                                , { cells = repeat 10 { color = (Just "black") } }
                                , { cells = repeat 10 { color = (Just "black") } }
                                , { cells = repeat 10 { color = (Just "black") } }
                                ]
                            }
            ]
        ]
