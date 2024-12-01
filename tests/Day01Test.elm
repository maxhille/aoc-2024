module Day01Test exposing (..)

import Day01 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 1 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            3   4
                            4   3
                            2   5
                            1   3
                            3   9
                            3   3
                            """
                in
                Expect.equal
                    (Ok 11)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 2 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            3   4
                            4   3
                            2   5
                            1   3
                            3   9
                            3   3
                            """
                in
                Expect.equal
                    (Ok 31)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            3   4
                            4   3
                            2   5
                            1   3
                            3   9
                            3   3
                            """
                in
                Expect.equal
                    (Ok
                        { left = [ 3, 4, 2, 1, 3, 3 ]
                        , right = [ 4, 3, 5, 3, 9, 3 ]
                        }
                    )
                    (Parser.run parser input)
        ]
