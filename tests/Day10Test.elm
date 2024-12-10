module Day10Test exposing (..)

import Day10 exposing (..)
import Expect
import Grid
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 10 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            89010123
                            78121874
                            87430965
                            96549874
                            45678903
                            32019012
                            01329801
                            10456732
                            """
                in
                Expect.equal
                    (Ok 36)
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
                            345
                            123
                            """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ 3, 4, 5 ]
                            , [ 1, 2, 3 ]
                            ]
                    )
                    (Parser.run parser input)
        ]
