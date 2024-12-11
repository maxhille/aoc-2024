module Day11 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


calculatePart1 : List Int -> Result String Int
calculatePart1 =
    List.map (Tuple.pair 25)
        >> countStones
        >> Ok


calculatePart2 : List Int -> Result String Int
calculatePart2 =
    List.map (Tuple.pair 75)
        >> countStones
        >> Ok


countStones : List ( Int, Int ) -> Int
countStones =
    countStonesHelp Dict.empty >> Tuple.second


countStonesHelp : Dict ( Int, Int ) Int -> List ( Int, Int ) -> ( Dict ( Int, Int ) Int, Int )
countStonesHelp dict stones =
    case stones of
        [] ->
            ( dict, 0 )

        ( steps, stone ) :: rest ->
            case Dict.get ( steps, stone ) dict of
                Just knownCount ->
                    countStonesHelp dict rest |> Tuple.mapSecond ((+) knownCount)

                Nothing ->
                    if steps == 0 then
                        countStonesHelp dict rest |> Tuple.mapSecond ((+) 1)

                    else
                        let
                            ( next, rest_ ) =
                                case split stone of
                                    Nothing ->
                                        if stone == 0 then
                                            ( ( steps - 1, 1 ), rest )

                                        else
                                            ( ( steps - 1, stone * 2024 ), rest )

                                    Just ( stone1, stone2 ) ->
                                        ( ( steps - 1, stone1 ), ( steps - 1, stone2 ) :: rest )

                            ( nextDict, nextCount ) =
                                countStonesHelp dict [ next ]
                        in
                        countStonesHelp (Dict.insert next nextCount nextDict) rest_ |> Tuple.mapSecond ((+) nextCount)


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
