module Day02 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing (Parser, Trailing(..))
import Puzzle exposing (Puzzle, inputParser, listParser)


type alias Report =
    List Level


type alias Level =
    Int


type Direction
    = Descending
    | Ascending
    | Undecided


calculatePart1 : List Report -> Result String Int
calculatePart1 reports =
    case reports of
        [] ->
            Err "reports list is empty"

        r1 :: rest ->
            partHelp False r1 rest []
                |> Result.map (List.filter identity >> List.length)


calculatePart2 : List Report -> Result String Int
calculatePart2 reports =
    case reports of
        [] ->
            Err "reports list is empty"

        r1 :: rest ->
            partHelp True r1 rest []
                |> Result.map (List.filter identity >> List.length)


partHelp : Bool -> Report -> List Report -> List Bool -> Result String (List Bool)
partHelp dampener report reports safeties =
    let
        safety =
            safe dampener report
    in
    case safety of
        Err msg ->
            Err msg

        Ok bool ->
            case reports of
                [] ->
                    Ok <| bool :: safeties

                next :: rest ->
                    partHelp dampener next rest (bool :: safeties)


safe : Bool -> Report -> Result String Bool
safe dampener levels =
    case levels of
        l1 :: l2 :: rest ->
            Ok <| safeHelp dampener l1 l2 Undecided rest

        _ ->
            Err "report does not have enough levels to assert safety"


safeHelp : Bool -> Level -> Level -> Direction -> List Level -> Bool
safeHelp dampener prev2 prev1 direction togo =
    let
        step l1 l2 =
            let
                diff =
                    l1 - l2
            in
            if abs diff <= 3 && abs diff > 0 then
                Just <|
                    if diff < 0 then
                        Ascending

                    else
                        Descending

            else
                Nothing
    in
    case togo of
        [] ->
            True

        next :: rest ->
            let
                step1 =
                    step prev2 prev1

                step2 =
                    step prev1 next
            in
            case direction of
                Undecided ->
                    case ( step1, step2, dampener ) of
                        ( Just _, Nothing, True ) ->
                            safeHelp False prev2 next Undecided rest
                                || safeHelp False prev1 next Undecided rest

                        ( Nothing, Just _, True ) ->
                            safeHelp False prev1 next Undecided rest
                                || safeHelp False prev2 next Undecided rest

                        ( Just direction1, Just direction2, _ ) ->
                            if direction1 == direction2 then
                                safeHelp dampener prev1 next direction2 rest

                            else if dampener then
                                safeHelp False prev1 next direction2 rest
                                    || (case step prev2 next of
                                            Just direction3 ->
                                                safeHelp False prev2 next direction3 rest

                                            Nothing ->
                                                False
                                       )
                                    || safeHelp False prev2 prev1 direction1 rest

                            else
                                False

                        ( _, _, _ ) ->
                            False

                Ascending ->
                    case ( step1, step2, dampener ) of
                        ( Just Ascending, Just Ascending, _ ) ->
                            safeHelp dampener prev1 next direction rest

                        ( Just Ascending, _, True ) ->
                            safeHelp False prev2 prev1 direction rest
                                || safeHelp False prev2 next direction rest

                        ( _, Just Ascending, True ) ->
                            safeHelp False prev1 next direction rest

                        ( _, _, _ ) ->
                            False

                Descending ->
                    case ( step1, step2, dampener ) of
                        ( Just Descending, Just Descending, _ ) ->
                            safeHelp dampener prev1 next direction rest

                        ( Just Descending, _, True ) ->
                            safeHelp False prev2 prev1 direction rest
                                || safeHelp False prev2 next direction rest

                        ( _, Just Descending, True ) ->
                            safeHelp False prev1 next direction rest

                        ( _, _, _ ) ->
                            False


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
