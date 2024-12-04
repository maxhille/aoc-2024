module Day04Test exposing (..)

import Day04 exposing (..)
import Expect
import Grid
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 3 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            MMMSXXMASM
                            MSAMXMSMSA
                            AMXSXMAAMM
                            MSAMASMSMX
                            XMASAMXAMM
                            XXAMMXXAMA
                            SMSMSASXSS
                            SAXAMASAAA
                            MAMMMXMMMM
                            MXMXAXMASX
                            """
                in
                Expect.equal
                    (Ok 18)
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
                            XMAS
                            XMAS
                            XMAS
                            """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ X, M, A, S ]
                            , [ X, M, A, S ]
                            , [ X, M, A, S ]
                            ]
                    )
                    (Parser.run parser input)
        ]
