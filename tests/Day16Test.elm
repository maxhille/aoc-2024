module Day16Test exposing (..)

import Day16 exposing (..)
import Expect
import Grid
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 16 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            ###############
                            #.......#....E#
                            #.#.###.#.###.#
                            #.....#.#...#.#
                            #.###.#####.#.#
                            #.#.#.......#.#
                            #.#.#####.###.#
                            #...........#.#
                            ###.#.#####.#.#
                            #...#.....#.#.#
                            #.#.#.###.#.#.#
                            #.....#...#.#.#
                            #.###.#.#.#.#.#
                            #S..#.....#...#
                            ###############
                            """
                in
                Expect.equal
                    (Ok 7036)
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
                            #################
                            #...#...#...#..E#
                            #.#.#.#.#.#.#.#.#
                            #.#.#.#...#...#.#
                            #.#.#.#.###.#.#.#
                            #...#.#.#.....#.#
                            #.#.#.#.#.#####.#
                            #.#...#.#.#.....#
                            #.#.#####.#.###.#
                            #.#.#.......#...#
                            #.#.###.#####.###
                            #.#.#...#.....#.#
                            #.#.#.#####.###.#
                            #.#.#.........#.#
                            #.#.#.#########.#
                            #S#.............#
                            #################
                            """
                in
                Expect.equal
                    (Ok 11048)
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
                            ###############
                            #.......#....E#
                            #.#.###.#.###.#
                            #.....#.#...#.#
                            #.###.#####.#.#
                            #.#.#.......#.#
                            #.#.#####.###.#
                            #...........#.#
                            ###.#.#####.#.#
                            #...#.....#.#.#
                            #.#.#.###.#.#.#
                            #.....#...#.#.#
                            #.###.#.#.#.#.#
                            #S..#.....#...#
                            ###############
                            """
                in
                Expect.equal
                    (Ok 45)
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
                            #################
                            #...#...#...#..E#
                            #.#.#.#.#.#.#.#.#
                            #.#.#.#...#...#.#
                            #.#.#.#.###.#.#.#
                            #...#.#.#.....#.#
                            #.#.#.#.#.#####.#
                            #.#...#.#.#.....#
                            #.#.#####.#.###.#
                            #.#.#.......#...#
                            #.#.###.#####.###
                            #.#.#...#.....#.#
                            #.#.#.#####.###.#
                            #.#.#.........#.#
                            #.#.#.#########.#
                            #S#.............#
                            #################
                            """
                in
                Expect.equal
                    (Ok 64)
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
                            ####
                            #S.#
                            #.E#
                            ####
                            """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ Wall, Wall, Wall, Wall ]
                            , [ Wall, Start, Empty, Wall ]
                            , [ Wall, Empty, End, Wall ]
                            , [ Wall, Wall, Wall, Wall ]
                            ]
                    )
                    (Parser.run parser input)
        ]
