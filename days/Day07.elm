module Day07 exposing (Equation, calculatePart1, calculatePart2, parser, puzzle)

import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type alias Equation =
    { value : Int
    , numbers : List Int
    }


calculatePart1 : List Equation -> Result String Int
calculatePart1 =
    List.filter canBeTrue >> List.map .value >> List.sum >> Ok


calculatePart2 : List Equation -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


canBeTrue : Equation -> Bool
canBeTrue { value, numbers } =
    canBeTrueHelp value 0 numbers


canBeTrueHelp : Int -> Int -> List Int -> Bool
canBeTrueHelp test cur numbers =
    case numbers of
        [] ->
            test == cur

        x :: xs ->
            canBeTrueHelp test (cur + x) xs || canBeTrueHelp test (cur * x) xs


parser : Parser (List Equation)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "\n"
        , spaces = Parser.chompWhile (\_ -> False)
        , item =
            Parser.succeed Equation
                |= Parser.int
                |. Parser.symbol ": "
                |= Parser.sequence
                    { start = ""
                    , end = ""
                    , separator = " "
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
