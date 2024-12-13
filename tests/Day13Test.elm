module Day13Test exposing (..)

import Day13 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 13 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            Button A: X+94, Y+34
                            Button B: X+22, Y+67
                            Prize: X=8400, Y=5400

                            Button A: X+26, Y+66
                            Button B: X+67, Y+21
                            Prize: X=12748, Y=12176

                            Button A: X+17, Y+86
                            Button B: X+84, Y+37
                            Prize: X=7870, Y=6450

                            Button A: X+69, Y+23
                            Button B: X+27, Y+71
                            Prize: X=18641, Y=10279
                            """
                in
                Expect.equal
                    (Ok 480)
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
                            Button A: X+94, Y+34
                            Button B: X+22, Y+67
                            Prize: X=8400, Y=5400

                            Button A: X+26, Y+66
                            Button B: X+67, Y+21
                            Prize: X=12748, Y=12176
                            """
                in
                Expect.equal
                    (Ok <|
                        [ { a = { x = 94, y = 34 }
                          , b = { x = 22, y = 67 }
                          , prize = { x = 8400, y = 5400 }
                          }
                        , { a = { x = 26, y = 66 }
                          , b = { x = 67, y = 21 }
                          , prize = { x = 12748, y = 12176 }
                          }
                        ]
                    )
                    (Parser.run parser input)
        ]
