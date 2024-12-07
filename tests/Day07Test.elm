module Day07Test exposing (..)

import Day07 exposing (..)
import Expect
import Parser
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
                            190: 10 19
                            3267: 81 40 27
                            83: 17 5
                            156: 15 6
                            7290: 6 8 6 15
                            161011: 16 10 13
                            192: 17 8 14
                            21037: 9 7 18 13
                            292: 11 6 16 20
                            """
                in
                Expect.equal
                    (Ok 3749)
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
                            190: 10 19
                            3267: 81 40 27
                            83: 17 5
                            156: 15 6
                            7290: 6 8 6 15
                            161011: 16 10 13
                            192: 17 8 14
                            21037: 9 7 18 13
                            292: 11 6 16 20
                            """
                in
                Expect.equal
                    (Ok 11387)
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
                            83: 17 5
                            156: 15 6
                            """
                in
                Expect.equal
                    (Ok <|
                        [ { value = 83, numbers = [ 17, 5 ] }
                        , { value = 156, numbers = [ 15, 6 ] }
                        ]
                    )
                    (Parser.run parser input)
        ]
