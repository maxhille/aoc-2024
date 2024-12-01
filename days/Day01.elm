module Day01 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Parser exposing ((|.), (|=), Parser, Trailing(..), spaces, succeed, symbol)
import Puzzle exposing (Puzzle)


type alias Lists =
    { left : List Int
    , right : List Int
    }


calculatePart1 : Lists -> Result String Int
calculatePart1 { left, right } =
    List.map2 (\l r -> abs (l - r)) (List.sort left) (List.sort right)
        |> List.sum
        |> Ok


calculatePart2 : Lists -> Result String Int
calculatePart2 _ =
    Err "implement"


parser : Parser Lists
parser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = pairParser
        , trailing = Optional
        }
        |> Parser.map
            (List.foldr
                (\( l, r ) lists ->
                    { lists
                        | left = l :: lists.left
                        , right = r :: lists.right
                    }
                )
                { left = [], right = [] }
            )


pairParser : Parser ( Int, Int )
pairParser =
    succeed Tuple.pair
        |= Parser.int
        |. symbol " "
        |. symbol " "
        |. symbol " "
        |= Parser.int


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
