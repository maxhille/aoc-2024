module Day16 exposing (Item(..), calculatePart1, calculatePart2, parser, puzzle)

import Day08 exposing (Antenna(..))
import Dict exposing (Dict)
import Grid exposing (Grid, Pos)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Maze =
    Grid Item


type Item
    = Wall
    | Empty
    | Start
    | End


type Direction
    = North
    | East
    | South
    | West


type alias Location =
    ( Pos, Char )


toLocation : ( Pos, Direction ) -> ( Pos, Char )
toLocation =
    Tuple.mapSecond
        (\direction ->
            case direction of
                North ->
                    'N'

                East ->
                    'E'

                South ->
                    'S'

                West ->
                    'W'
        )


fromLocation : ( Pos, Char ) -> ( Pos, Direction )
fromLocation =
    Tuple.mapSecond
        (\char ->
            case char of
                'N' ->
                    North

                'E' ->
                    East

                'S' ->
                    South

                _ ->
                    West
        )


calculatePart1 : Maze -> Result String Int
calculatePart1 maze =
    case ( Grid.find ((==) Start) maze, Grid.find ((==) End) maze ) of
        ( [ start ], [ end ] ) ->
            fill ( start, East ) end maze

        _ ->
            Err "maze has zero or multiple starts/ends"


fill : ( Pos, Direction ) -> Pos -> Maze -> Result String Int
fill deer end maze =
    fillHelp maze (Dict.singleton (toLocation deer) 0) Dict.empty
        |> Dict.filter (\( location, _ ) _ -> location == end)
        |> Dict.values
        |> List.minimum
        |> Result.fromMaybe "end not reached"


fillHelp : Maze -> Dict Location Int -> Dict Location Int -> Dict Location Int
fillHelp maze deers paths =
    if Dict.isEmpty deers then
        paths

    else
        let
            isWall pos =
                Grid.get pos maze |> Maybe.map ((==) Wall) |> Maybe.withDefault False

            newDeers =
                deers
                    |> Dict.foldl
                        (\location score acc ->
                            let
                                ( ( x, y ), direction ) =
                                    fromLocation location

                                south =
                                    ( ( ( x, y + 1 )
                                      , South
                                      )
                                    , score
                                        + 1
                                        + (case direction of
                                            North ->
                                                2000

                                            South ->
                                                0

                                            East ->
                                                1000

                                            West ->
                                                1000
                                          )
                                    )

                                north =
                                    ( ( ( x, y - 1 )
                                      , North
                                      )
                                    , score
                                        + 1
                                        + (case direction of
                                            North ->
                                                0

                                            South ->
                                                2000

                                            East ->
                                                1000

                                            West ->
                                                1000
                                          )
                                    )

                                east =
                                    ( ( ( x + 1, y )
                                      , East
                                      )
                                    , score
                                        + 1
                                        + (case direction of
                                            North ->
                                                1000

                                            South ->
                                                1000

                                            East ->
                                                0

                                            West ->
                                                2000
                                          )
                                    )

                                west =
                                    ( ( ( x - 1, y )
                                      , West
                                      )
                                    , score
                                        + 1
                                        + (case direction of
                                            North ->
                                                1000

                                            South ->
                                                1000

                                            East ->
                                                2000

                                            West ->
                                                0
                                          )
                                    )
                            in
                            (case direction of
                                North ->
                                    [ north, east, west ]

                                South ->
                                    [ south, east, west ]

                                East ->
                                    [ south, north, east ]

                                West ->
                                    [ south, north, west ]
                            )
                                |> List.filter (Tuple.first >> Tuple.first >> (not << isWall))
                                |> List.filter (\( deer, score1 ) -> Dict.get (toLocation deer) paths |> Maybe.map (\score2 -> score2 > score1) |> Maybe.withDefault True)
                                |> List.map (Tuple.mapFirst toLocation)
                                |> Dict.fromList
                                |> mergeLowScore acc
                        )
                        Dict.empty
        in
        fillHelp maze newDeers (mergeLowScore deers paths)


mergeLowScore : Dict Location Int -> Dict Location Int -> Dict Location Int
mergeLowScore dict1 dict2 =
    Dict.merge (\k v acc -> Dict.insert k v acc)
        (\k v1 v2 acc -> Dict.insert k (min v1 v2) acc)
        (\k v acc -> Dict.insert k v acc)
        dict1
        dict2
        Dict.empty


calculatePart2 : Maze -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


parser : Parser Maze
parser =
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
                , item = Puzzle.charParser [ ( '#', Wall ), ( '.', Empty ), ( 'S', Start ), ( 'E', End ) ]
                , trailing = Parser.Mandatory
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
