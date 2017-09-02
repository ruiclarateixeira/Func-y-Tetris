module MyUtilsTests exposing (..)

import Test exposing (..)
import Expect
import MyUtils exposing (..)
import Pieces exposing (..)


all : Test
all =
    describe "Utils test suite"
        [ describe "find largest coordinates"
            [ test "empty array doesn't explode" <|
                \() ->
                    Expect.equal (MyUtils.findLargestCoordinates []) ( 0, 0 )
            , test "Lshape coordinates works" <|
                \() ->
                    Expect.equal (MyUtils.findLargestCoordinates [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ) ]) ( 1, 2 )
            , test "Tshape coordinates works" <|
                \() ->
                    Expect.equal (MyUtils.findLargestCoordinates [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]) ( 2, 1 )
            , test "Ishape coordinates works" <|
                \() ->
                    Expect.equal (MyUtils.findLargestCoordinates [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]) ( 0, 3 )
            , test "Sshape coordinates works" <|
                \() ->
                    Expect.equal (MyUtils.findLargestCoordinates [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]) ( 2, 1 )
            , test "Oshape coordinates works" <|
                \() ->
                    Expect.equal (MyUtils.findLargestCoordinates [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]) ( 1, 1 )
            ]
        , describe "matrix rotation"
            [ test "rotate square matrix" <|
                \() ->
                    let
                        og =
                            [ [ 1, 2, 3, 4 ]
                            , [ 5, 6, 7, 8 ]
                            , [ 9, 0, 1, 2 ]
                            , [ 3, 4, 5, 6 ]
                            ]

                        ex =
                            [ [ Just 4, Just 8, Just 2, Just 6 ]
                            , [ Just 3, Just 7, Just 1, Just 5 ]
                            , [ Just 2, Just 6, Just 0, Just 4 ]
                            , [ Just 1, Just 5, Just 9, Just 3 ]
                            ]
                    in
                        Expect.equal (rotateSquareMatrix og) ex
            , test "rotate og LShape" <|
                \() ->
                    let
                        og =
                            [ [ LShape, LShape, None ]
                            , [ LShape, None, None ]
                            , [ LShape, None, None ]
                            ]

                        ex =
                            [ [ Just None, Just None, Just None ]
                            , [ Just LShape, Just None, Just None ]
                            , [ Just LShape, Just LShape, Just LShape ]
                            ]
                    in
                        Expect.equal (rotateSquareMatrix og) ex
            ]
        ]
