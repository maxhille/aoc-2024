module Day11 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


calculatePart1 : List Int -> Result String Int
calculatePart1 initialStones =
    List.foldl (\_ stones -> List.concatMap blink stones) initialStones (List.range 1 25)
        |> List.length
        |> Ok


calculatePart2 : List Int -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


blink : Int -> List Int
blink stone =
    case split stone of
        Nothing ->
            if stone == 0 then
                [ 1 ]

            else
                [ stone * 2024 ]

        Just ( stone1, stone2 ) ->
            [ stone1, stone2 ]


split : Int -> Maybe ( Int, Int )
split stone =
    let
        str =
            String.fromInt stone

        len =
            String.length str
    in
    if modBy 2 len == 0 then
        Maybe.map2 Tuple.pair
            (String.left (len // 2) str |> String.toInt)
            (String.right (len // 2) str |> String.toInt)

    else
        Nothing


parser : Parser (List Int)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = " "
        , spaces = Parser.chompWhile (always False)
        , item = Parser.int
        , trailing = Parser.Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
