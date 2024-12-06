module Day06 exposing (Direction(..), Lab, Position(..), calculatePart1, calculatePart2, parser, puzzle)

import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)
import Set


type alias Lab =
    Grid Position


type Position
    = Space
    | Obstacle
    | Guard Direction


type Direction
    = Up
    | Left
    | Down
    | Right


calculatePart1 : Lab -> Result String Int
calculatePart1 =
    trace >> Result.map (Set.fromList >> Set.size)


trace : Lab -> Result String (List ( Int, Int ))
trace lab =
    let
        guardPos =
            Grid.find ((==) (Guard Up)) lab
    in
    case guardPos of
        [ pos ] ->
            Grid.set pos Space lab
                |> traceHelp pos Up [ pos ]
                |> Ok

        [] ->
            Err "No Guard found"

        _ ->
            Err "Multiple Guards found"


traceHelp : ( Int, Int ) -> Direction -> List ( Int, Int ) -> Lab -> List ( Int, Int )
traceHelp pos direction result lab =
    let
        nextPos =
            next pos direction
    in
    case Grid.get nextPos lab of
        Nothing ->
            pos :: result

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
            traceHelp pos newDirection result lab

        Just _ ->
            traceHelp nextPos direction (pos :: result) lab


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


calculatePart2 : Lab -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


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
