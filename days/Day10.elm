module Day10 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)
import Set


type alias Location =
    { xy : ( Int, Int )
    , height : Int
    }


calculatePart1 : Grid Int -> Result String Int
calculatePart1 grid =
    trailHeads grid |> List.map (score grid) |> List.sum |> Ok


calculatePart2 : Grid Int -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


trailHeads : Grid Int -> List Location
trailHeads =
    let
        height =
            0
    in
    Grid.find ((==) height) >> List.map (\xy -> { xy = xy, height = height })


score : Grid Int -> Location -> Int
score grid trailHead =
    scoreHelp grid [ trailHead ]
        |> List.map .xy
        |> Set.fromList
        |> Set.size


scoreHelp : Grid Int -> List Location -> List Location
scoreHelp grid xys =
    let
        ( moving, done ) =
            xys |> List.partition (.height >> (/=) 9)
    in
    if moving == [] then
        done

    else
        scoreHelp grid (done ++ List.concatMap (adjacent grid) moving)


adjacent : Grid Int -> Location -> List Location
adjacent grid location =
    let
        ( x, y ) =
            location.xy
    in
    [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
        |> List.filterMap (\xy -> Grid.get xy grid |> Maybe.map (\height -> { xy = xy, height = height }))
        |> List.filter (.height >> (==) (location.height + 1))


parser : Parser (Grid Int)
parser =
    Parser.succeed Grid.fromLists
        |= Parser.sequence
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
                    , item = Puzzle.singleDigitParser
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
