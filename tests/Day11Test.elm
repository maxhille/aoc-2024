module Day11Test exposing (..)

import Day11 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 11 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            125 17
                            """
                in
                Expect.equal
                    (Ok 55312)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            3 4 5
                            """
                in
                Expect.equal
                    (Ok <|
                        [ 3, 4, 5 ]
                    )
                    (Parser.run parser input)
        ]
