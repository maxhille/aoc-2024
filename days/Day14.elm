module Day14 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Robot =
    { p : { x : Int, y : Int }
    , v : { x : Int, y : Int }
    }


type alias Space =
    { width : Int
    , height : Int
    }


calculatePart1 : Space -> List Robot -> Result String Int
calculatePart1 space robots =
    simulate 100 space robots
        |> safetyFactor space
        |> Ok


calculatePart2 : Space -> List Robot -> Result String Int
calculatePart2 _ =
    Puzzle.notImplemented


safetyFactor : Space -> List Robot -> Int
safetyFactor space =
    List.foldr
        (\robot quadrants ->
            let
                top =
                    robot.p.y < space.height // 2

                bot =
                    robot.p.y > space.height // 2

                right =
                    robot.p.x < (space.width // 2)

                left =
                    robot.p.x > space.width // 2
            in
            if top && right then
                { quadrants | topRight = robot :: quadrants.topRight }

            else if bot && right then
                { quadrants | botRight = robot :: quadrants.botRight }

            else if bot && left then
                { quadrants | botLeft = robot :: quadrants.botLeft }

            else if top && left then
                { quadrants | topLeft = robot :: quadrants.topLeft }

            else
                quadrants
        )
        { topLeft = [], topRight = [], botRight = [], botLeft = [] }
        >> (\quadrants -> List.length quadrants.topLeft * List.length quadrants.topRight * List.length quadrants.botRight * List.length quadrants.botLeft)


simulate : Int -> Space -> List Robot -> List Robot
simulate seconds space robots =
    if seconds == 0 then
        robots

    else
        simulate (seconds - 1) space (List.map (advance space) robots)


advance : Space -> Robot -> Robot
advance space { p, v } =
    { p =
        { x = p.x + v.x |> modBy space.width
        , y = p.y + v.y |> modBy space.height
        }
    , v = v
    }


parser : Parser (List Robot)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.spaces
        , item = machineParser
        , trailing = Parser.Optional
        }


machineParser : Parser Robot
machineParser =
    let
        signedInt =
            Parser.oneOf
                [ Parser.succeed negate
                    |. Parser.symbol "-"
                    |= Parser.int
                , Parser.int
                ]
    in
    Parser.succeed Robot
        |= (Parser.succeed (\x y -> { x = x, y = y })
                |. Parser.symbol "p="
                |= signedInt
                |. Parser.symbol ","
                |= signedInt
           )
        |. Parser.symbol " "
        |= (Parser.succeed (\x y -> { x = x, y = y })
                |. Parser.symbol "v="
                |= signedInt
                |. Parser.symbol ","
                |= signedInt
           )


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen (calculatePart1 { width = 101, height = 103 })
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen (calculatePart2 { width = 101, height = 103 })
    }
