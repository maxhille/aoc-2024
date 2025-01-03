module Day06 exposing (Direction(..), Lab, Position(..), calculatePart1, calculatePart2, parser, puzzle)

import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)
import Set exposing (Set)


type alias Lab =
    Grid Position


type Position
    = Space
    | Obstacle
    | Guard Direction


type Path
    = Exit (Set ( ( Int, Int ), Int ))
    | Loop


type Direction
    = Up
    | Left
    | Down
    | Right


calculatePart1 : Lab -> Result String Int
calculatePart1 =
    trace
        >> Result.andThen
            (\path ->
                case path of
                    Loop ->
                        Err "The guard ran a loop"

                    Exit positionDirections ->
                        positionDirections
                            |> Set.map Tuple.first
                            |> Set.size
                            |> Ok
            )


calculatePart2 : Lab -> Result String Int
calculatePart2 lab =
    let
        guardPos =
            Grid.find ((==) (Guard Up)) lab
                |> List.head
                |> Result.fromMaybe "Could not locate guard for removal"

        part1Path =
            trace
                >> Result.andThen
                    (\path ->
                        case path of
                            Loop ->
                                Err "The guard ran a loop"

                            Exit positionDirections ->
                                positionDirections
                                    |> Set.map Tuple.first
                                    |> Ok
                    )
    in
    Result.map2
        (\guardPos_ path ->
            path
                |> Set.toList
                |> List.filter ((/=) guardPos_)
                |> List.foldl
                    (\( x, y ) count ->
                        case trace (Grid.set ( x, y ) Obstacle lab) of
                            Ok Loop ->
                                count + 1

                            _ ->
                                count
                    )
                    0
        )
        guardPos
        (part1Path lab)


directionToInt : Direction -> Int
directionToInt direction =
    case direction of
        Up ->
            0

        Right ->
            1

        Down ->
            2

        Left ->
            3


trace : Lab -> Result String Path
trace lab =
    let
        guardPos =
            Grid.find ((==) (Guard Up)) lab
    in
    case guardPos of
        [ pos ] ->
            Grid.set pos Space lab
                |> traceHelp pos Up (Set.singleton ( pos, directionToInt Up ))
                |> Ok

        [] ->
            Err "No Guard found"

        _ ->
            Err "Multiple Guards found"


traceHelp : ( Int, Int ) -> Direction -> Set ( ( Int, Int ), Int ) -> Lab -> Path
traceHelp pos direction result lab =
    let
        nextPos =
            next pos direction
    in
    if Set.member ( nextPos, directionToInt direction ) result then
        Loop

    else
        case Grid.get nextPos lab of
            Nothing ->
                Exit <| Set.insert ( pos, directionToInt direction ) result

            Just Obstacle ->
                let
                    newDirection =
                        case direction of
                            Up ->
                                Right

                            Right ->
                                Down

                            Down ->
                                Left

                            Left ->
                                Up
                in
                traceHelp pos newDirection (Set.insert ( pos, directionToInt direction ) result) lab

            Just _ ->
                traceHelp nextPos direction (Set.insert ( pos, directionToInt direction ) result) lab


next : ( Int, Int ) -> Direction -> ( Int, Int )
next ( x, y ) direction =
    case direction of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


parser : Parser Lab
parser =
    Parser.succeed Grid.fromLists
        |= Parser.sequence
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
                        Parser.oneOf
                            [ Parser.succeed Space
                                |. Parser.symbol "."
                            , Parser.succeed Obstacle
                                |. Parser.symbol "#"
                            , Parser.succeed (Guard Up)
                                |. Parser.symbol "^"
                            , Parser.getChompedString (Parser.chompIf ((/=) '\n'))
                                |> Parser.andThen (\c -> Parser.problem <| "Unexpected position symbol: " ++ c)
                            ]
                    , trailing = Parser.Optional
                    }
            , trailing = Parser.Optional
            }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
