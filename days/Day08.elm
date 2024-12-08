module Day08 exposing (Antenna(..), antinodes, calculatePart1, calculatePart2, parser, puzzle)

import Dict
import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, cartesian)
import Set exposing (Set)


type Antenna
    = Empty
    | Freq Char


calculatePart1 : Grid Antenna -> Result String Int
calculatePart1 grid =
    grid
        |> Grid.positionedFold
            (\( pos, ant ) dict ->
                case ant of
                    Empty ->
                        dict

                    Freq f ->
                        Dict.update f
                            (\poss_ ->
                                case poss_ of
                                    Just poss ->
                                        Just <| pos :: poss

                                    Nothing ->
                                        Just [ pos ]
                            )
                            dict
            )
            Dict.empty
        >> Dict.values
        >> List.foldl (\antennas nodes -> Set.union (antinodes antennas) nodes) Set.empty
        >> Set.filter (inBounds grid)
        >> Set.size
        >> Ok


inBounds : Grid a -> ( Int, Int ) -> Bool
inBounds grid ( x, y ) =
    grid |> Grid.size >> (\{ xs, ys } -> x < xs && x >= 0 && y < ys && y >= 0)


antinodes : List ( Int, Int ) -> Set ( Int, Int )
antinodes antennas =
    cartesian antennas antennas
        |> List.foldl
            (\( ( x1, y1 ), ( x2, y2 ) ) acc ->
                if x1 == x2 && y1 == y2 then
                    acc

                else
                    let
                        dx =
                            x1 - x2

                        dy =
                            y1 - y2
                    in
                    ( x1 + dx, y1 + dy ) :: ( x2 - dx, y2 - dy ) :: acc
            )
            []
        |> Set.fromList


calculatePart2 : Grid Antenna -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


parser : Parser (Grid Antenna)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "\n"
        , spaces = Parser.chompWhile (\_ -> False)
        , item =
            Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , spaces = Parser.chompWhile (\_ -> False)
                , item =
                    Parser.succeed ()
                        |. Parser.chompIf (\c -> Char.isAlphaNum c || c == '.')
                        |> Parser.getChompedString
                        |> Parser.andThen
                            (\str ->
                                case String.uncons str of
                                    Just ( '.', _ ) ->
                                        Parser.succeed Empty

                                    Just ( c, _ ) ->
                                        Parser.succeed (Freq c)

                                    Nothing ->
                                        Parser.problem "could not get char from string"
                            )
                , trailing = Parser.Optional
                }
        , trailing = Parser.Optional
        }
        |> Parser.map Grid.fromLists


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
