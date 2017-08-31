module MyUtilsTests exposing (..)

import Test exposing (..)
import Expect
import MyUtils exposing (..)


all : Test
all =
    describe "Utils test suite"
        [ describe "get largest coordinates"
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
        ]
