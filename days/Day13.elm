module Day13 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Button =
    { x : Int
    , y : Int
    }


type alias Prize =
    { x : Int
    , y : Int
    }


type alias Machine =
    { a : Button
    , b : Button
    , prize : { x : Int, y : Int }
    }


calculatePart1 : List Machine -> Result String Int
calculatePart1 =
    List.filterMap minTokens >> List.sum >> Ok


minTokens : Machine -> Maybe Int
minTokens machine =
    minTokensHelp machine 0


minTokensHelp : Machine -> Int -> Maybe Int
minTokensHelp { a, b, prize } bPresses =
    let
        restX =
            prize.x - b.x * bPresses

        restY =
            prize.y - b.y * bPresses
    in
    if bPresses > 100 then
        Nothing

    else if modBy a.x restX == 0 && modBy a.y restY == 0 then
        let
            aPresses1 =
                restX // a.x

            aPresses2 =
                restY // a.y
        in
        if aPresses1 /= aPresses2 || aPresses1 > 100 then
            minTokensHelp { a = a, b = b, prize = prize } (bPresses + 1)

        else
            Just (aPresses1 * 3 + bPresses * 1)

    else
        minTokensHelp { a = a, b = b, prize = prize } (bPresses + 1)


calculatePart2 : List Machine -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


parser : Parser (List Machine)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.spaces
        , item = machineParser
        , trailing = Parser.Optional
        }


machineParser : Parser Machine
machineParser =
    Parser.succeed Machine
        |= (Parser.succeed Button
                |. Parser.symbol "Button A: X+"
                |= Parser.int
                |. Parser.symbol ", Y+"
                |= Parser.int
                |. Parser.symbol "\n"
           )
        |= (Parser.succeed Button
                |. Parser.symbol "Button B: X+"
                |= Parser.int
                |. Parser.symbol ", Y+"
                |= Parser.int
                |. Parser.symbol "\n"
           )
        |= (Parser.succeed Prize
                |. Parser.symbol "Prize: X="
                |= Parser.int
                |. Parser.symbol ", Y="
                |= Parser.int
           )


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
