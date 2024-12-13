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
    List.filterMap minTokens
        >> List.filter (\{ a, b } -> a <= 100 && b <= 100)
        >> List.map (\{ a, b } -> a * 3 + b * 1)
        >> List.sum
        >> Ok


calculatePart2 : List Machine -> Result String Int
calculatePart2 =
    List.map
        (\machine ->
            let
                prize =
                    machine.prize
            in
            { machine
                | prize =
                    { prize
                        | x = prize.x + 10000000000000
                        , y = prize.y + 10000000000000
                    }
            }
        )
        >> List.filterMap minTokens
        >> List.map (\{ a, b } -> a * 3 + b * 1)
        >> List.sum
        >> Ok


minTokens : Machine -> Maybe { a : Int, b : Int }
minTokens { a, b, prize } =
    let
        aPresses =
            cramerX { a1 = a.x, b1 = b.x, a2 = a.y, b2 = b.y, c1 = prize.x, c2 = prize.y }
                |> toInt

        bPresses =
            cramerY { a1 = a.x, b1 = b.x, a2 = a.y, b2 = b.y, c1 = prize.x, c2 = prize.y }
                |> toInt
    in
    Maybe.map2 (\aPresses_ bPresses_ -> { a = aPresses_, b = bPresses_ }) aPresses bPresses


toInt : Float -> Maybe Int
toInt float =
    let
        int =
            round float
    in
    if toFloat int == float then
        Just int

    else
        Nothing


cramerX : { a1 : Int, a2 : Int, b1 : Int, b2 : Int, c1 : Int, c2 : Int } -> Float
cramerX { a1, a2, b1, b2, c1, c2 } =
    toFloat (c1 * b2 - b1 * c2) / toFloat (a1 * b2 - b1 * a2)


cramerY : { a1 : Int, a2 : Int, b1 : Int, b2 : Int, c1 : Int, c2 : Int } -> Float
cramerY { a1, a2, b1, b2, c1, c2 } =
    toFloat (a1 * c2 - c1 * a2) / toFloat (a1 * b2 - b1 * a2)


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
