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
    calculate grid False |> Ok


calculatePart2 : Grid Antenna -> Result String Int
calculatePart2 grid =
    calculate grid True |> Ok


calculate : Grid Antenna -> Bool -> Int
calculate grid harmonics =
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
        >> List.foldl
            (\antennas nodes ->
                let
                    inBounds ( x, y ) =
                        grid |> Grid.size >> (\{ xs, ys } -> x < xs && x >= 0 && y < ys && y >= 0)
                in
                Set.union (antinodes inBounds harmonics antennas) nodes
            )
            Set.empty
        >> Set.size


antinodes : (( Int, Int ) -> Bool) -> Bool -> List ( Int, Int ) -> Set ( Int, Int )
antinodes inBounds harmonics antennas =
    cartesian antennas antennas
        |> List.filter (\( xy1, xy2 ) -> xy1 /= xy2)
        |> List.foldl
            (\( ( x1, y1 ), ( x2, y2 ) ) acc ->
                let
                    ( dx, dy ) =
                        ( x1 - x2, y1 - y2 )
                in
                antinodesHelp inBounds
                    harmonics
                    ( dx, dy )
                    ( x1, y1 )
                    ( x2, y2 )
                    (if harmonics then
                        0

                     else
                        1
                    )
                    Set.empty
                    |> Set.union acc
            )
            Set.empty


antinodesHelp : (( Int, Int ) -> Bool) -> Bool -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Int -> Set ( Int, Int ) -> Set ( Int, Int )
antinodesHelp inBounds harmonics ( dx, dy ) ( x1, y1 ) ( x2, y2 ) n acc =
    let
        resonance1 =
            ( x1 + n * dx, y1 + n * dy )

        resonance2 =
            ( x2 - n * dx, y2 - n * dy )
    in
    case ( inBounds resonance1, inBounds resonance2 ) of
        ( False, False ) ->
            acc

        ( True, False ) ->
            if harmonics then
                antinodesHelp inBounds harmonics ( dx, dy ) ( x1, y1 ) ( x2, y2 ) (n + 1) (acc |> Set.insert resonance1)

            else
                acc
                    |> Set.insert resonance1

        ( False, True ) ->
            if harmonics then
                antinodesHelp inBounds harmonics ( dx, dy ) ( x1, y1 ) ( x2, y2 ) (n + 1) (acc |> Set.insert resonance2)

            else
                acc |> Set.insert resonance2

        ( True, True ) ->
            if harmonics then
                antinodesHelp inBounds harmonics ( dx, dy ) ( x1, y1 ) ( x2, y2 ) (n + 1) (acc |> Set.insert resonance1 |> Set.insert resonance2)

            else
                acc |> Set.insert resonance1 |> Set.insert resonance2


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
