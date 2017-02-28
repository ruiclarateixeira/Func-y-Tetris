port module Main exposing (..)

import BoardTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)


main : TestProgram
main =
    run emit (describe "all tests" [ BoardTests.all ])


port emit : ( String, Value ) -> Cmd msg
