module Day02 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing (Parser, Trailing(..))
import Puzzle exposing (Puzzle, inputParser, listParser)
import Set


type alias Report =
    List Level


type alias Level =
    Int


type Direction
    = Descending
    | Ascending


calculatePart1 : List Report -> Result String Int
calculatePart1 reports =
    case reports of
        [] ->
            Err "reports list is empty"

        r1 :: rest ->
            part1Help r1 rest []
                |> Result.map (List.filter identity >> List.length)


part1Help : Report -> List Report -> List Bool -> Result String (List Bool)
part1Help report reports safeties =
    let
        safety =
            safe report
    in
    case safety of
        Err msg ->
            Err msg

        Ok bool ->
            case reports of
                [] ->
                    Ok <| bool :: safeties

                next :: rest ->
                    part1Help next rest (bool :: safeties)


safe : Report -> Result String Bool
safe levels =
    case levels of
        l1 :: l2 :: rest ->
            let
                direction =
                    if l1 < l2 then
                        Ascending

                    else
                        Descending
            in
            Ok <| safeHelp l1 direction (l2 :: rest)

        _ ->
            Err "report does not have enough levels to assert safety"


safeHelp : Level -> Direction -> List Level -> Bool
safeHelp last direction togo =
    case togo of
        [] ->
            True

        next :: rest ->
            case direction of
                Ascending ->
                    if Set.member (next - last) (Set.fromList [ 1, 2, 3 ]) then
                        safeHelp next direction rest

                    else
                        False

                Descending ->
                    if Set.member (last - next) (Set.fromList [ 1, 2, 3 ]) then
                        safeHelp next direction rest

                    else
                        False


calculatePart2 : List Report -> Result String Int
calculatePart2 _ =
    Err "implement"


parser : Parser (List Report)
parser =
    inputParser reportParser


reportParser : Parser (List Level)
reportParser =
    listParser
        { take = [ Parser.int ]
        , skip = [ Parser.symbol " " ]
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
