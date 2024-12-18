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
        >> List.map (\region -> (perimeter region |> List.length) * area region)
        >> List.sum
        >> Ok


calculatePart2 : Grid Char -> Result String Int
calculatePart2 =
    toRegions
        >> List.map (\region -> corners region * area region)
        >> List.sum
        >> Ok


area : Region -> Int
area =
    .xys >> Set.size


bounds : Region -> { xMin : Int, xMax : Int, yMin : Int, yMax : Int }
bounds =
    .xys
        >> Set.toList
        >> List.unzip
        >> (\( xs, ys ) ->
                { xMin = List.minimum xs |> Maybe.withDefault 1000
                , xMax = List.maximum xs |> Maybe.withDefault 0
                , yMin = List.minimum ys |> Maybe.withDefault 1000
                , yMax = List.maximum ys |> Maybe.withDefault 0
                }
           )


corners : Region -> Int
corners region =
    let
        { xMin, xMax, yMin, yMax } =
            bounds region
    in
    cartesian (List.range (xMin - 1) (xMax + 1)) (List.range (yMin - 1) (yMax + 1))
        |> List.foldl (\xy acc -> acc + cornerCount xy region.xys) 0


cornerCount : Pos -> Set Pos -> Int
cornerCount ( x, y ) xys =
    let
        -- x#
        -- ##
        topLeftConcave =
            [ ( False, ( x, y ) ), ( True, ( x + 1, y ) ), ( True, ( x, y + 1 ) ), ( True, ( x + 1, y + 1 ) ) ]

        -- #x
        -- ##
        topRightConcave =
            [ ( True, ( x - 1, y ) ), ( False, ( x, y ) ), ( True, ( x - 1, y + 1 ) ), ( True, ( x, y + 1 ) ) ]

        -- ##
        -- #x
        bottomRightConcave =
            [ ( True, ( x - 1, y - 1 ) ), ( True, ( x, y - 1 ) ), ( True, ( x - 1, y ) ), ( False, ( x, y ) ) ]

        -- ##
        -- x#
        bottomLeftConcave =
            [ ( True, ( x, y - 1 ) ), ( True, ( x + 1, y - 1 ) ), ( False, ( x, y ) ), ( True, ( x + 1, y ) ) ]

        -- x.
        -- .#
        topLeftConvex =
            [ ( False, ( x + 1, y ) ), ( False, ( x, y + 1 ) ), ( True, ( x + 1, y + 1 ) ) ]

        -- .x
        -- #.
        topRightConvex =
            [ ( False, ( x - 1, y ) ), ( True, ( x - 1, y + 1 ) ), ( False, ( x, y + 1 ) ) ]

        -- #.
        -- .x
        bottomRightConvex =
            [ ( True, ( x - 1, y - 1 ) ), ( False, ( x, y - 1 ) ), ( False, ( x - 1, y ) ) ]

        -- .#
        -- x.
        bottomLeftConvex =
            [ ( False, ( x, y - 1 ) ), ( True, ( x + 1, y - 1 ) ), ( False, ( x + 1, y ) ) ]
    in
    [ topLeftConcave
    , topRightConcave
    , bottomLeftConcave
    , bottomRightConcave
    , topLeftConvex
    , topRightConvex
    , bottomLeftConvex
    , bottomRightConvex
    ]
        |> List.filter (List.all (\( contains, xy_ ) -> Set.member xy_ xys == contains))
        |> List.length


perimeter : Region -> List Touch
perimeter region =
    let
        { xMin, xMax, yMin, yMax } =
            bounds region
    in
    cartesian (List.range (xMin - 1) (xMax + 1)) (List.range (yMin - 1) (yMax + 1))
        |> List.foldl
            (\xy acc ->
                if not (Set.member xy region.xys) then
                    touches xy region.xys ++ acc

                else
                    acc
            )
            []


type Direction
    = Vertical
    | Horizontal


type alias Touch =
    { pos : Pos, dir : Direction }


touches : Pos -> Set Pos -> List Touch
touches ( x_, y_ ) xys =
    List.filterMap
        (\( xy_, dir ) ->
            if Set.member xy_ xys then
                Just { pos = ( x_, y_ ), dir = dir }

            else
                Nothing
        )
        [ ( ( x_ + 1, y_ ), Vertical ), ( ( x_ - 1, y_ ), Vertical ), ( ( x_, y_ + 1 ), Horizontal ), ( ( x_, y_ - 1 ), Horizontal ) ]


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


parser : Parser (Grid Char)
parser =
    gridParser (charParser Char.isAlphaNum)


charParser : (Char -> Bool) -> Parser Char
charParser isChar =
    Parser.getChompedString (Parser.chompIf isChar)
        |> Parser.andThen
            (\str ->
                case str |> String.toList >> List.head of
                    Nothing ->
                        Parser.problem "could not parse char"

                    Just char ->
                        Parser.succeed char
            )


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
