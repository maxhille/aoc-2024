module Day09Test exposing (..)

import Day09 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 9 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            2333133121414131402
                            """
                in
                Expect.equal
                    (Ok 1928)
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
                            2333133121414131402
                            """
                in
                Expect.equal
                    (Ok 2858)
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
                            12345
                            """
                in
                Expect.equal
                    (Ok <|
                        [ 1, 2, 3, 4, 5 ]
                    )
                    (Parser.run parser input)
        ]
