module Day03Test exposing (..)

import Day03 exposing (..)
import Expect
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
                            xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
                            """
                in
                Expect.equal
                    (Ok 161)
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
                            xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
                            """
                in
                Expect.equal
                    (Ok 48)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen calculatePart2
                    )
        , test "parser - Part 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
                            """
                in
                Expect.equal
                    (Ok
                        [ [ Mul 2 4, Mul 5 5, Mul 11 8, Mul 8 5 ]
                        ]
                    )
                    (Parser.run parser input)
        , test "parser - Part 2" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
                            """
                in
                Expect.equal
                    (Ok
                        [ [ Mul 2 4, Dont, Mul 5 5, Mul 11 8, Do, Mul 8 5 ]
                        ]
                    )
                    (Parser.run parser input)
        ]
