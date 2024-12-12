module Day12 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Grid exposing (Grid, Pos)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, cartesian, charParser, gridParser)
import Set exposing (Set)


type alias Region =
    { char : Char
    , xys : Set Pos
    }


calculatePart1 : Grid Char -> Result String Int
calculatePart1 =
    toRegions
        >> List.map (\region -> perimeter region * area region)
        >> List.sum
        >> Ok


area : Region -> Int
area =
    .xys >> Set.size


perimeter : Region -> Int
perimeter { xys } =
    let
        { xMin, xMax, yMin, yMax } =
            xys
                |> Set.toList
                |> List.unzip
                |> (\( xs, ys ) ->
                        { xMin = List.minimum xs |> Maybe.withDefault 1000
                        , xMax = List.maximum xs |> Maybe.withDefault 0
                        , yMin = List.minimum ys |> Maybe.withDefault 1000
                        , yMax = List.maximum ys |> Maybe.withDefault 0
                        }
                   )
    in
    cartesian (List.range (xMin - 1) (xMax + 1)) (List.range (yMin - 1) (yMax + 1))
        |> List.foldl
            (\xy sum ->
                if not (Set.member xy xys) then
                    sum + touches xy xys

                else
                    sum
            )
            0


calculatePart2 : Grid Char -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


toRegions : Grid Char -> List Region
toRegions grid =
    grid
        |> Grid.positionedFold
            (\( xy, char ) { done, regions } ->
                if Set.member xy done then
                    { done = done, regions = regions }

                else
                    let
                        xys =
                            fill char xy grid
                    in
                    { done = Set.union done xys, regions = { char = char, xys = xys } :: regions }
            )
            { done = Set.empty, regions = [] }
        >> .regions


fill : Char -> Pos -> Grid Char -> Set Pos
fill char pos grid =
    fillHelp char grid Set.empty (Set.singleton pos)


fillHelp : Char -> Grid Char -> Set Pos -> Set Pos -> Set Pos
fillHelp char grid filled front =
    if Set.isEmpty front then
        filled

    else
        let
            newFront =
                Set.foldl (\xy acc -> Set.union (neighbours char grid xy |> Set.fromList) acc) Set.empty front
                    |> Set.filter (\xy -> not <| Set.member xy filled)
        in
        fillHelp char grid (Set.union filled front) newFront


neighbours : Char -> Grid Char -> Pos -> List Pos
neighbours char grid ( x, y ) =
    [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filter (\xy -> Grid.get xy grid |> Maybe.map ((==) char) |> Maybe.withDefault False)


touches : Pos -> Set Pos -> Int
touches ( x_, y_ ) xys =
    List.filter (\xy_ -> Set.member xy_ xys) [ ( x_ + 1, y_ ), ( x_ - 1, y_ ), ( x_, y_ + 1 ), ( x_, y_ - 1 ) ]
        |> List.length


parser : Parser (Grid Char)
parser =
    gridParser (charParser Char.isAlphaNum)


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
