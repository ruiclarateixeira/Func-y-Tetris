port module Main exposing (..)

import BoardTests
import MyUtilsTests
import PiecesTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)


main : TestProgram
main =
    run emit (describe "All Board Tests" [ BoardTests.all, MyUtilsTests.all, PiecesTests.all ])


port emit : ( String, Value ) -> Cmd msg
