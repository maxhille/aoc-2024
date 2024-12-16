module Day15 exposing (Direction(..), Item(..), calculatePart1, calculatePart2, parser, puzzle)

import Grid exposing (Grid, Pos)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Warehouse =
    Grid Item


type Item
    = Wall
    | Empty
    | Robot
    | Box


type Direction
    = Up
    | Right
    | Down
    | Left


calculatePart1 : { warehouse : Warehouse, moves : List Direction } -> Result String Int
calculatePart1 { warehouse, moves } =
    apply moves warehouse |> Result.map gpsSum


apply : List Direction -> Warehouse -> Result String Warehouse
apply directions warehouse =
    case Grid.find ((==) Robot) warehouse of
        [ robot ] ->
            case directions of
                [] ->
                    Ok warehouse

                direction :: rest ->
                    case move direction robot warehouse of
                        Just warehouse_ ->
                            apply rest warehouse_

                        Nothing ->
                            apply rest warehouse

        _ ->
            Err "zero or multiple robots in this warehouse"


move : Direction -> Pos -> Warehouse -> Maybe Warehouse
move direction ( x, y ) warehouse =
    let
        newPos =
            case direction of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )
    in
    case Maybe.map2 Tuple.pair (Grid.get ( x, y ) warehouse) (Grid.get newPos warehouse) of
        Just ( Robot, Empty ) ->
            warehouse
                |> Grid.set ( x, y ) Empty
                |> Grid.set newPos Robot
                |> Just

        Just ( Box, Empty ) ->
            warehouse
                |> Grid.set newPos Box
                |> Just

        Just ( Robot, Box ) ->
            warehouse
                |> move direction newPos
                |> Maybe.map (Grid.set ( x, y ) Empty)
                |> Maybe.map (Grid.set newPos Robot)

        Just ( Box, Box ) ->
            warehouse
                |> move direction newPos
                |> Maybe.map (Grid.set newPos Box)

        _ ->
            Nothing


gpsSum : Warehouse -> Int
gpsSum =
    Grid.positionedFold
        (\( ( x, y ), item ) sum ->
            case item of
                Box ->
                    sum + x + 100 * y

                _ ->
                    sum
        )
        0


calculatePart2 : { warehouse : Warehouse, moves : List Direction } -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


parser : Parser { warehouse : Grid Item, moves : List Direction }
parser =
    Parser.succeed (\grid seq -> { warehouse = grid, moves = seq })
        |= warehouseParser
        |= directionsParser


warehouseParser : Parser (Grid Item)
warehouseParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "\n"
        , spaces = Parser.chompWhile (always False)
        , item =
            Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , spaces = Parser.chompWhile (always False)
                , item = charParser [ ( '#', Wall ), ( '.', Empty ), ( 'O', Box ), ( '@', Robot ) ]
                , trailing = Parser.Mandatory
                }
        , trailing = Parser.Optional
        }
        |> Parser.map (List.filter (not << List.isEmpty) >> Grid.fromLists)


charParser : List ( Char, b ) -> Parser b
charParser list =
    list
        |> List.map (\( char, b ) -> Parser.symbol (String.fromChar char) |> Parser.map (always b))
        |> Parser.oneOf


directionsParser : Parser (List Direction)
directionsParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.spaces
        , item = charParser [ ( '^', Up ), ( '<', Left ), ( '>', Right ), ( 'v', Down ) ]
        , trailing = Parser.Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
