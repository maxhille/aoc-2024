module Day05 exposing (Instructions, calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Instructions =
    { rules : List ( Int, Int )
    , updates : List (List Int)
    }


calculatePart1 : Instructions -> Result String Int
calculatePart1 { rules, updates } =
    updates
        |> List.filter (\update -> update == sort rules update)
        |> List.map (\xs -> List.drop (List.length xs // 2) xs)
        |> List.filterMap List.head
        |> List.sum
        |> Ok


calculatePart2 : Instructions -> Result String Int
calculatePart2 { rules, updates } =
    updates
        |> List.filter (\update -> update /= sort rules update)
        |> List.map (sort rules)
        |> List.map (\xs -> List.drop (List.length xs // 2) xs)
        |> List.filterMap List.head
        |> List.sum
        |> Ok


sort : List ( Int, Int ) -> List Int -> List Int
sort rules =
    List.sortWith
        (\update1 update2 ->
            if List.member ( update1, update2 ) rules then
                LT

            else
                GT
        )


parser : Parser Instructions
parser =
    Parser.succeed Instructions
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.chompWhile (\_ -> False)
            , item =
                Parser.succeed Tuple.pair
                    |= Parser.int
                    |. Parser.symbol "|"
                    |= Parser.int
                    |. Parser.symbol "\n"
            , trailing = Parser.Optional
            }
        |. Parser.symbol "\n"
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = "\n"
            , spaces = Parser.chompWhile (\_ -> False)
            , item =
                Parser.sequence
                    { start = ""
                    , end = ""
                    , separator = ","
                    , spaces = Parser.chompWhile (\_ -> False)
                    , item = Parser.int
                    , trailing = Parser.Optional
                    }
            , trailing = Parser.Optional
            }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
