module Day15Test exposing (..)

import Day15 exposing (..)
import Expect
import Grid
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 15 tests"
        [ test "Part 1 - Large example" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            ##########
                            #..O..O.O#
                            #......O.#
                            #.OO..O.O#
                            #..O@..O.#
                            #O#..O...#
                            #O..O..O.#
                            #.OO.O.OO#
                            #....O...#
                            ##########

                            <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                            vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                            ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                            <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                            ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                            ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                            >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                            <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                            ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                            v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
                            """
                in
                Expect.equal
                    (Ok 10092)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart1
                    )
        , test "Part 1 - Small example" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            ########
                            #..O.O.#
                            ##@.O..#
                            #...O..#
                            #.#.O..#
                            #...O..#
                            #......#
                            ########

                            <^^>>>vv<v>>v<<
                            """
                in
                Expect.equal
                    (Ok 2028)
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
                            ####
                            #O.#
                            #.@#
                            ####

                            <v
                            ^>
                            """
                in
                Expect.equal
                    (Ok <|
                        { warehouse =
                            Grid.fromLists
                                [ [ Wall, Wall, Wall, Wall ]
                                , [ Wall, Box, Empty, Wall ]
                                , [ Wall, Empty, Robot, Wall ]
                                , [ Wall, Wall, Wall, Wall ]
                                ]
                        , moves = [ Left, Down, Up, Right ]
                        }
                    )
                    (Parser.run parser input)
        ]
