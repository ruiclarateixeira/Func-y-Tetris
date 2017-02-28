module BoardTests exposing (..)

import Test exposing (..)
import Array exposing (length, get, filter)
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
                        Expect.equal (length (Array.filter lengthFilter board.rows)) 0
            ]
        ]
