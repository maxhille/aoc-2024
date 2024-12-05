module Day05Test exposing (..)

import Day05 exposing (..)
import Expect
import Parser
import Test exposing (..)
import Util exposing (sanitize)


suite : Test
suite =
    describe "Day 5 tests"
        [ test "Part 1 - Example 1" <|
            \_ ->
                let
                    input =
                        sanitize <|
                            """
                            47|53
                            97|13
                            97|61
                            97|47
                            75|29
                            61|13
                            75|53
                            29|13
                            97|29
                            53|29
                            61|53
                            97|53
                            61|29
                            47|13
                            75|47
                            97|75
                            47|61
                            75|61
                            47|29
                            75|13
                            53|13

                            75,47,61,53,29
                            97,61,53,29,13
                            75,29,13
                            75,97,47,61,53
                            61,13,29
                            97,13,75,29,47
                            """
                in
                Expect.equal
                    (Ok 143)
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
                            47|29
                            75|13
                            53|13

                            75,47,61,53,29
                            97,61,53,29,13
                            """
                in
                Expect.equal
                    (Ok <|
                        { rules =
                            [ ( 47, 29 )
                            , ( 75, 13 )
                            , ( 53, 13 )
                            ]
                        , updates =
                            [ [ 75, 47, 61, 53, 29 ]
                            , [ 97, 61, 53, 29, 13 ]
                            ]
                        }
                    )
                    (Parser.run parser input)
        ]
