module Day12Test exposing (..)

import Day12 exposing (..)
import Expect
import Grid
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 12 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            AAAA
                            BBCD
                            BBCC
                            EEEC
                            """
                in
                Expect.equal
                    (Ok 140)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 1 - Example 2" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            OOOOO
                            OXOXO
                            OOOOO
                            OXOXO
                            OOOOO
                            """
                in
                Expect.equal
                    (Ok 772)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 1 - Example 3" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            RRRRIICCFF
                            RRRRIICCCF
                            VVRRRCCFFF
                            VVRCCCJFFF
                            VVVVCJJCFE
                            VVIVCCJJEE
                            VVIIICJJEE
                            MIIIIIJJEE
                            MIIISIJEEE
                            MMMISSJEEE
                            """
                in
                Expect.equal
                    (Ok 1930)
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
                            AAAA
                            BBCD
                            BBCC
                            EEEC
                            """
                in
                Expect.equal
                    (Ok 80)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "Part 2 - Example 2" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            OOOOO
                            OXOXO
                            OOOOO
                            OXOXO
                            OOOOO
                            """
                in
                Expect.equal
                    (Ok 436)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "Part 2 - Example 3" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            AAAAAA
                            AAABBA
                            AAABBA
                            ABBAAA
                            ABBAAA
                            AAAAAA
                            """
                in
                Expect.equal
                    (Ok 368)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "Part 2 - Example 4" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            RRRRIICCFF
                            RRRRIICCCF
                            VVRRRCCFFF
                            VVRCCCJFFF
                            VVVVCJJCFE
                            VVIVCCJJEE
                            VVIIICJJEE
                            MIIIIIJJEE
                            MIIISIJEEE
                            MMMISSJEEE
                            """
                in
                Expect.equal
                    (Ok 1206)
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
                            AB
                            CD
                            """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ 'A', 'B' ]
                            , [ 'C', 'D' ]
                            ]
                    )
                    (Parser.run parser input)
        ]
