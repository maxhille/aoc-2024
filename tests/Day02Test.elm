module Day02Test exposing (..)

import Day02 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 2 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            7 6 4 2 1
                            1 2 7 8 9
                            9 7 6 2 1
                            1 3 2 4 5
                            8 6 4 4 1
                            1 3 6 7 9
                            """
                in
                Expect.equal
                    (Ok 2)
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
                            1 3 53 2
                            1 2
                            """
                in
                Expect.equal
                    (Ok
                        [ [ 1, 3, 53, 2 ]
                        , [ 1, 2 ]
                        ]
                    )
                    (Parser.run parser input)
        ]
