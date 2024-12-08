module Day08Test exposing (..)

import Day08 exposing (..)
import Expect
import Grid
import Parser
import Set
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 7 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            ............
                            ........0...
                            .....0......
                            .......0....
                            ....0.......
                            ......A.....
                            ............
                            ............
                            ........A...
                            .........A..
                            ............
                            ............
                            """
                in
                Expect.equal
                    (Ok 14)
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
                            ............
                            ........0...
                            .....0......
                            .......0....
                            ....0.......
                            ......A.....
                            ............
                            ............
                            ........A...
                            .........A..
                            ............
                            ............
                            """
                in
                Expect.equal
                    (Ok 34)
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
                            T.........
                            ...T......
                            .T........
                            ..........
                            ..........
                            ..........
                            ..........
                            ..........
                            ..........
                            ..........
                            """
                in
                Expect.equal
                    (Ok 9)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "Part 2 - Debug 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            .T..
                            ....
                            .T..
                            ....
                            """
                in
                Expect.equal
                    (Ok 2)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "antinodes 1" <|
            \_ ->
                let
                    antennas =
                        [ ( 4, 3 ), ( 5, 5 ) ]
                in
                Expect.equal
                    (Set.fromList [ ( 3, 1 ), ( 6, 7 ) ])
                    (antinodes (\_ -> True) False antennas)
        , test "antinodes 2" <|
            \_ ->
                let
                    antennas =
                        [ ( 5, 5 ), ( 4, 3 ) ]
                in
                Expect.equal
                    (Set.fromList [ ( 3, 1 ), ( 6, 7 ) ])
                    (antinodes (\_ -> True) False antennas)
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            .B
                            A.
                            """
                in
                Expect.equal
                    (Ok <|
                        Grid.fromLists
                            [ [ Empty, Freq 'B' ]
                            , [ Freq 'A', Empty ]
                            ]
                    )
                    (Parser.run parser input)
        ]
