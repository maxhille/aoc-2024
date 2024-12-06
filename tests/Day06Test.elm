module Day06Test exposing (..)

import Day06 exposing (..)
import Expect
import Grid
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 6 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            ....#.....
                            .........#
                            ..........
                            ..#.......
                            .......#..
                            ..........
                            .#..^.....
                            ........#.
                            #.........
                            ......#...
                            """
                in
                Expect.equal
                    (Ok 41)
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
                            .#
                            .^
                            """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ Space, Obstacle ]
                            , [ Space, Guard Up ]
                            ]
                    )
                    (Parser.run parser input)
        ]
