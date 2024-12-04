module Day04 exposing (Letter(..), calculatePart1, calculatePart2, parser, puzzle)

import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle, inputParser, listParser)


type Letter
    = X
    | M
    | A
    | S
    | Z


part1Masks : List (Grid Letter)
part1Masks =
    [ [ [ X, M, A, S ] ]
    , [ [ S, A, M, X ] ]
    , [ [ X ]
      , [ M ]
      , [ A ]
      , [ S ]
      ]
    , [ [ S ]
      , [ A ]
      , [ M ]
      , [ X ]
      ]
    , [ [ X ]
      , [ Z, M ]
      , [ Z, Z, A ]
      , [ Z, Z, Z, S ]
      ]
    , [ [ S ]
      , [ Z, A ]
      , [ Z, Z, M ]
      , [ Z, Z, Z, X ]
      ]
    , [ [ Z, Z, Z, X ]
      , [ Z, Z, M ]
      , [ Z, A ]
      , [ S ]
      ]
    , [ [ Z, Z, Z, S ]
      , [ Z, Z, A ]
      , [ Z, M ]
      , [ X ]
      ]
    ]
        |> List.map Grid.fromLists


part2Masks : List (Grid Letter)
part2Masks =
    [ [ [ M, Z, S ]
      , [ Z, A ]
      , [ M, Z, S ]
      ]
    , [ [ M, Z, M ]
      , [ Z, A ]
      , [ S, Z, S ]
      ]
    , [ [ S, Z, M ]
      , [ Z, A ]
      , [ S, Z, M ]
      ]
    , [ [ S, Z, S ]
      , [ Z, A ]
      , [ M, Z, M ]
      ]
    ]
        |> List.map Grid.fromLists


calculatePart1 : Grid Letter -> Result String Int
calculatePart1 =
    calculate part1Masks >> Ok


calculatePart2 : Grid Letter -> Result String Int
calculatePart2 =
    calculate part2Masks >> Ok


calculate : List (Grid Letter) -> Grid Letter -> Int
calculate masks grid =
    let
        { xs, ys } =
            Grid.size grid
    in
    cartesian (List.range 0 (xs - 1)) (List.range 0 (ys - 1))
        |> List.map
            (\xy ->
                List.foldl
                    (\mask count ->
                        if containsAt xy mask grid then
                            count + 1

                        else
                            count
                    )
                    0
                    masks
            )
        |> List.sum


containsAt : ( Int, Int ) -> Grid Letter -> Grid Letter -> Bool
containsAt ( x, y ) mask grid =
    Grid.toPositionedList mask
        |> List.all
            (\( ( dx, dy ), letter ) ->
                if letter == Z then
                    True

                else
                    case Grid.get ( x + dx, y + dy ) grid of
                        Nothing ->
                            False

                        Just letter_ ->
                            letter_ == letter
            )


cartesian : List a -> List b -> List ( a, b )
cartesian as_ bs =
    List.concatMap (\x -> List.map (\y -> ( x, y )) bs) as_


parser : Parser (Grid Letter)
parser =
    inputParser opsParser |> Parser.map Grid.fromLists


opsParser : Parser (List Letter)
opsParser =
    listParser
        { take =
            [ Parser.oneOf <|
                List.map
                    (\( letter, char ) ->
                        Parser.succeed letter |. Parser.chompIf ((==) char)
                    )
                    [ ( X, 'X' ), ( M, 'M' ), ( A, 'A' ), ( S, 'S' ) ]
            ]
        , skip =
            [ Parser.getChompedString (Parser.chompIf ((/=) '\n'))
                |> Parser.andThen (\c -> Parser.problem ("unexpected char " ++ c))
            ]
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
