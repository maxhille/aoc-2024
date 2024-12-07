module Day07 exposing (Equation, calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Equation =
    { value : Int
    , numbers : List Int
    }


calculatePart1 : List Equation -> Result String Int
calculatePart1 =
    List.filter (canBeTrue [ (+), (*) ]) >> List.map .value >> List.sum >> Ok


calculatePart2 : List Equation -> Result String Int
calculatePart2 =
    let
        concat x y =
            -- uncleanly ignore the failure to parse the int back after concat. we know it'll work
            String.fromInt x ++ String.fromInt y |> String.toInt |> Maybe.withDefault -1
    in
    List.filter (canBeTrue [ (+), (*), concat ]) >> List.map .value >> List.sum >> Ok


canBeTrue : List (Int -> Int -> Int) -> Equation -> Bool
canBeTrue ops { value, numbers } =
    canBeTrueHelp ops value 0 numbers


canBeTrueHelp : List (Int -> Int -> Int) -> Int -> Int -> List Int -> Bool
canBeTrueHelp ops test cur numbers =
    case numbers of
        [] ->
            test == cur

        x :: xs ->
            ops
                |> List.any (\op -> canBeTrueHelp ops test (op cur x) xs)


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
