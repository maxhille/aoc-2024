module Day03 exposing (Op(..), calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, inputParser)


type Op
    = Mul Int Int
    | Dont
    | Do


calculatePart1 : List (List Op) -> Result String Int
calculatePart1 =
    List.concat
        >> List.map
            (\op ->
                case op of
                    Mul x y ->
                        x * y

                    _ ->
                        0
            )
        >> List.sum
        >> Ok


calculatePart2 : List (List Op) -> Result String Int
calculatePart2 =
    List.concat
        >> List.foldl
            (\op acc ->
                case op of
                    Mul x y ->
                        { acc
                            | sum =
                                acc.sum
                                    + (if acc.enabled then
                                        x * y

                                       else
                                        0
                                      )
                        }

                    Do ->
                        { acc | enabled = True }

                    Dont ->
                        { acc | enabled = False }
            )
            { sum = 0, enabled = True }
        >> .sum
        >> Ok


parser : Parser (List (List Op))
parser =
    inputParser opsParser


opsParser : Parser (List Op)
opsParser =
    Parser.loop [] opsParserHelp


opsParserHelp : List Op -> Parser (Parser.Step (List Op) (List Op))
opsParserHelp revOps =
    Parser.oneOf
        [ Parser.backtrackable <|
            Parser.succeed (\x y -> Parser.Loop (Mul x y :: revOps))
                |. Parser.symbol "mul("
                |= Parser.int
                |. Parser.symbol ","
                |= Parser.int
                |. Parser.symbol ")"
        , Parser.backtrackable <|
            Parser.succeed (Parser.Loop (Dont :: revOps))
                |. Parser.symbol "don't()"
        , Parser.backtrackable <|
            Parser.succeed (Parser.Loop (Do :: revOps))
                |. Parser.symbol "do()"
        , Parser.succeed (Parser.Loop revOps)
            |. Parser.chompIf (\_ -> True)
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revOps))
        ]


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
