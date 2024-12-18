module Day15 exposing (Direction(..), Item(..), calculatePart1, calculatePart2, parser, puzzle)

import Array
import Day08 exposing (Antenna(..))
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
    | LeftBox
    | RightBox


type Direction
    = Up
    | Right
    | Down
    | Left


calculatePart1 : { warehouse : Warehouse, moves : List Direction } -> Result String Int
calculatePart1 { warehouse, moves } =
    apply moves warehouse |> Result.map gpsSum


calculatePart2 : { warehouse : Warehouse, moves : List Direction } -> Result String Int
calculatePart2 { warehouse, moves } =
    widen warehouse
        |> apply moves
        |> Result.map gpsSum


widen : Warehouse -> Warehouse
widen =
    let
        mapRow =
            Array.toList
                >> List.concatMap
                    (\item ->
                        case item of
                            Wall ->
                                [ Wall, Wall ]

                            Empty ->
                                [ Empty, Empty ]

                            Robot ->
                                [ Robot, Empty ]

                            Box ->
                                [ LeftBox, RightBox ]

                            _ ->
                                []
                    )
                >> Array.fromList
    in
    Array.map mapRow


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
move direction here warehouse =
    let
        ( x, y ) =
            here

        next =
            case direction of
                Up ->
                    ( x, y - 1 )

                Left ->
                    ( x - 1, y )

                Right ->
                    ( x + 1, y )

                Down ->
                    ( x, y + 1 )
    in
    case Grid.get here warehouse of
        Just Robot ->
            case Maybe.map (\center -> ( center, direction )) (Grid.get next warehouse) of
                Just ( Empty, _ ) ->
                    warehouse
                        |> Grid.set here Empty
                        |> Grid.set next Robot
                        |> Just

                Just ( Box, _ ) ->
                    warehouse
                        |> move direction next
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Robot)

                Just ( RightBox, Left ) ->
                    warehouse
                        |> move direction ( x - 2, y )
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Robot)

                Just ( RightBox, Up ) ->
                    warehouse
                        |> move direction ( x - 1, y - 1 )
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Robot)

                Just ( RightBox, Down ) ->
                    warehouse
                        |> move direction ( x - 1, y + 1 )
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Robot)

                Just ( LeftBox, Right ) ->
                    warehouse
                        |> move direction ( x + 1, y )
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Robot)

                Just ( LeftBox, Up ) ->
                    warehouse
                        |> move direction ( x, y - 1 )
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Robot)

                Just ( LeftBox, Down ) ->
                    warehouse
                        |> move direction ( x, y + 1 )
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Robot)

                _ ->
                    Nothing

        Just Box ->
            case Grid.get next warehouse of
                Just Empty ->
                    warehouse
                        |> Grid.set here Empty
                        |> Grid.set next Box
                        |> Just

                Just Box ->
                    warehouse
                        |> move direction next
                        |> Maybe.map (Grid.set here Empty)
                        |> Maybe.map (Grid.set next Box)

                _ ->
                    Nothing

        Just LeftBox ->
            case direction of
                Left ->
                    case Grid.get next warehouse of
                        Just RightBox ->
                            warehouse
                                |> move direction ( x - 2, y )
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set here RightBox)
                                |> Maybe.map (Grid.set next LeftBox)

                        Just Empty ->
                            warehouse
                                |> Grid.set ( x + 1, y ) Empty
                                |> Grid.set here RightBox
                                |> Grid.set next LeftBox
                                |> Just

                        _ ->
                            Nothing

                Right ->
                    case Grid.get (next |> (\( x_, y_ ) -> ( x_ + 1, y_ ))) warehouse of
                        Just LeftBox ->
                            warehouse
                                |> move direction ( x + 2, y )
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 2, y ) RightBox)

                        Just Empty ->
                            warehouse
                                |> Grid.set ( x, y ) Empty
                                |> Grid.set ( x + 1, y ) LeftBox
                                |> Grid.set ( x + 2, y ) RightBox
                                |> Just

                        _ ->
                            Nothing

                Up ->
                    case Maybe.map2 Tuple.pair (Grid.get next warehouse) (Grid.get (next |> (\( x_, y_ ) -> ( x_ + 1, y_ ))) warehouse) of
                        Just ( RightBox, LeftBox ) ->
                            warehouse
                                |> move Up ( x - 1, y - 1 )
                                |> Maybe.andThen (move direction ( x + 1, y - 1 ))
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y - 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y - 1 ) RightBox)

                        Just ( LeftBox, _ ) ->
                            warehouse
                                |> move Up ( x, y - 1 )
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y - 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y - 1 ) RightBox)

                        Just ( RightBox, Empty ) ->
                            warehouse
                                |> move Up ( x - 1, y - 1 )
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y - 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y - 1 ) RightBox)

                        Just ( Empty, LeftBox ) ->
                            warehouse
                                |> move Up ( x + 1, y - 1 )
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y - 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y - 1 ) RightBox)

                        Just ( Empty, Empty ) ->
                            warehouse
                                |> Grid.set ( x, y ) Empty
                                |> Grid.set ( x + 1, y ) Empty
                                |> Grid.set ( x, y - 1 ) LeftBox
                                |> Grid.set ( x + 1, y - 1 ) RightBox
                                |> Just

                        _ ->
                            Nothing

                Down ->
                    case Maybe.map2 Tuple.pair (Grid.get next warehouse) (Grid.get (next |> (\( x_, y_ ) -> ( x_ + 1, y_ ))) warehouse) of
                        Just ( RightBox, LeftBox ) ->
                            warehouse
                                |> move direction ( x - 1, y + 1 )
                                |> Maybe.andThen (move direction ( x + 1, y + 1 ))
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y + 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y + 1 ) RightBox)

                        Just ( LeftBox, RightBox ) ->
                            warehouse
                                |> move direction ( x, y + 1 )
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y + 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y + 1 ) RightBox)

                        Just ( Empty, LeftBox ) ->
                            warehouse
                                |> move direction ( x + 1, y + 1 )
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y + 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y + 1 ) RightBox)

                        Just ( RightBox, Empty ) ->
                            warehouse
                                |> move direction ( x - 1, y + 1 )
                                |> Maybe.map (Grid.set ( x, y ) Empty)
                                |> Maybe.map (Grid.set ( x + 1, y ) Empty)
                                |> Maybe.map (Grid.set ( x, y + 1 ) LeftBox)
                                |> Maybe.map (Grid.set ( x + 1, y + 1 ) RightBox)

                        Just ( Empty, Empty ) ->
                            warehouse
                                |> Grid.set ( x, y ) Empty
                                |> Grid.set ( x + 1, y ) Empty
                                |> Grid.set ( x, y + 1 ) LeftBox
                                |> Grid.set ( x + 1, y + 1 ) RightBox
                                |> Just

                        _ ->
                            Nothing

        _ ->
            Nothing


gpsSum : Warehouse -> Int
gpsSum =
    Grid.positionedFold
        (\( ( x, y ), item ) sum ->
            case item of
                Box ->
                    sum + x + 100 * y

                LeftBox ->
                    sum + x + 100 * y

                _ ->
                    sum
        )
        0


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
                , item = Puzzle.charParser [ ( '#', Wall ), ( '.', Empty ), ( 'O', Box ), ( '@', Robot ) ]
                , trailing = Parser.Mandatory
                }
        , trailing = Parser.Optional
        }
        |> Parser.map (List.filter (not << List.isEmpty) >> Grid.fromLists)


directionsParser : Parser (List Direction)
directionsParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.spaces
        , item = Puzzle.charParser [ ( '^', Up ), ( '<', Left ), ( '>', Right ), ( 'v', Down ) ]
        , trailing = Parser.Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
