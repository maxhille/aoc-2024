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
        , test "Part 2 - Example 1" <|
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
                    (Ok 4)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "Part 2 - Debugging 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            87 90 91 92 96 93
                            """
                in
                Expect.equal
                    (Ok 1)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "Part 2 - Debugging 2" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            84 83 80 81 80
                            """
                in
                Expect.equal
                    (Ok 1)
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
