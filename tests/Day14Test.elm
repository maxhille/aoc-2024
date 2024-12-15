module Day14Test exposing (..)

import Day14 exposing (..)
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
                            p=0,4 v=3,-3
                            p=6,3 v=-1,-3
                            p=10,3 v=-1,2
                            p=2,0 v=2,-1
                            p=0,0 v=1,3
                            p=3,0 v=-2,-2
                            p=7,6 v=-1,-3
                            p=3,0 v=-1,-2
                            p=9,3 v=2,3
                            p=7,3 v=-1,2
                            p=2,4 v=2,-3
                            p=9,5 v=-3,-3
                            """
                in
                Expect.equal
                    (Ok 12)
                    (Parser.run parser input
                        |> Result.mapError Parser.deadEndsToString
                        |> Result.andThen (calculatePart1 { width = 11, height = 7 })
                    )
        , test "parser" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            p=0,4 v=3,-3
                            p=6,3 v=-1,-3
                            """
                in
                Expect.equal
                    (Ok <|
                        [ { p = { x = 0, y = 4 }, v = { x = 3, y = -3 } }
                        , { p = { x = 6, y = 3 }, v = { x = -1, y = -3 } }
                        ]
                    )
                    (Parser.run parser input)
        ]
