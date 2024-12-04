module Day04 exposing (Letter(..), calculatePart1, calculatePart2, parser, puzzle)

import Dict
import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, inputParser, listParser)


type Letter
    = X
    | M
    | A
    | S


type alias Transcriber =
    { count : Int
    , next : Letter
    }


transcribe : Letter -> Transcriber -> Transcriber
transcribe letter tr =
    if tr.next == letter then
        case letter of
            X ->
                { tr | next = M }

            M ->
                { tr | next = A }

            A ->
                { tr | next = S }

            S ->
                { tr | next = X, count = tr.count + 1 }

    else if letter == X then
        { tr | next = M }

    else
        { tr | next = X }


calculatePart1 : Grid Letter -> Result String Int
calculatePart1 grid =
    let
        { xs, ys } =
            Grid.size grid

        transcribeList =
            List.foldl transcribe { next = X, count = 0 }

        rowsForward =
            List.map (\y -> List.map (\x -> ( x, y )) (List.range 0 (xs - 1))) (List.range 0 (ys - 1))

        rowsBackward =
            rowsForward |> List.map List.reverse

        colsForward =
            List.map (\x -> List.map (\y -> ( x, y )) (List.range 0 (ys - 1))) (List.range 0 (xs - 1))

        colsBackward =
            colsForward |> List.map List.reverse

        diag1Backward =
            List.range 0 (xs - 1)
                |> List.map (\x -> List.map (\y -> ( x, y )) (List.range 0 (ys - 1)))
                |> List.concat
                |> List.foldl
                    (\( x, y ) dict ->
                        Dict.update (x - y)
                            (\xys_ ->
                                case xys_ of
                                    Just xys ->
                                        Just <| ( x, y ) :: xys

                                    Nothing ->
                                        Just [ ( x, y ) ]
                            )
                            dict
                    )
                    Dict.empty
                |> Dict.values

        diag1Forward =
            diag1Backward
                |> List.map List.reverse

        diag2Backward =
            List.range 0 (xs - 1)
                |> List.map (\x -> List.map (\y -> ( x, y )) (List.range 0 (ys - 1)))
                |> List.concat
                |> List.foldl
                    (\( x, y ) dict ->
                        Dict.update (x + y)
                            (\xys_ ->
                                case xys_ of
                                    Just xys ->
                                        Just <| ( x, y ) :: xys

                                    Nothing ->
                                        Just [ ( x, y ) ]
                            )
                            dict
                    )
                    Dict.empty
                |> Dict.values

        diag2Forward =
            diag2Backward
                |> List.map List.reverse
    in
    [ rowsForward
    , rowsBackward
    , colsForward
    , colsBackward
    , diag1Forward
    , diag1Backward
    , diag2Forward
    , diag2Backward
    ]
        |> List.concat
        |> List.map
            (List.filterMap (\xy -> Grid.get xy grid)
                >> transcribeList
                >> .count
            )
        |> List.sum
        |> Ok


calculatePart2 : Grid Letter -> Result String Int
calculatePart2 _ =
    Err "not implemented"


parser : Parser (Grid Letter)
parser =
    inputParser opsParser |> Parser.map Grid.fromLists


opsParser : Parser (List Letter)
opsParser =
    listParser
        { take =
            [ Parser.oneOf <|
                List.map
                    (\( letter, char ) ->
                        Parser.succeed letter |. Parser.chompIf ((==) char)
                    )
                    [ ( X, 'X' ), ( M, 'M' ), ( A, 'A' ), ( S, 'S' ) ]
            ]
        , skip =
            [ Parser.getChompedString (Parser.chompIf ((/=) '\n'))
                |> Parser.andThen (\c -> Parser.problem ("unexpected char " ++ c))
            ]
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
