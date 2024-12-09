module Day09 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Diskmap =
    List Int


type Block
    = Free
    | File Int


calculatePart1 : Diskmap -> Result String Int
calculatePart1 =
    toBlocks >> defrag >> checksum >> Ok


calculatePart2 : Diskmap -> Result String Int
calculatePart2 =
    Puzzle.notImplemented


defrag : Array Block -> Array Block
defrag array =
    defragHelp 0 (Array.length array - 1) array


defragHelp : Int -> Int -> Array Block -> Array Block
defragHelp n m array =
    if n >= m then
        array

    else
        case ( Array.get n array, Array.get m array ) of
            ( Just Free, Just Free ) ->
                defragHelp n (m - 1) array

            ( Just _, Just Free ) ->
                defragHelp (n + 1) m array

            ( Just Free, Just file ) ->
                defragHelp (n + 1) (m - 1) (array |> Array.set n file |> Array.set m Free)

            ( Just _, Just _ ) ->
                defragHelp (n + 1) m array

            ( _, _ ) ->
                Debug.todo "something was wrong with the indexes"


checksum : Array Block -> Int
checksum =
    Array.foldl
        (\block { i, sum } ->
            { i = i + 1
            , sum =
                case block of
                    Free ->
                        sum

                    File id ->
                        sum + (i * id)
            }
        )
        { i = 0, sum = 0 }
        >> .sum


toBlocks : Diskmap -> Array Block
toBlocks =
    List.foldl
        (\int { id, free, blocks } ->
            if free then
                { id = id
                , free = False
                , blocks = blocks ++ List.repeat int Free
                }

            else
                { id = id + 1
                , free = True
                , blocks = blocks ++ List.repeat int (File id)
                }
        )
        { id = 0
        , free = False
        , blocks = []
        }
        >> .blocks
        >> Array.fromList


parser : Parser Diskmap
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.chompWhile (always False)
        , item = singleDigitParser
        , trailing = Parser.Optional
        }


singleDigitParser : Parser Int
singleDigitParser =
    (Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf (always True)
    )
        |> Parser.andThen
            (\str ->
                case String.toInt str of
                    Just int ->
                        Parser.succeed int

                    Nothing ->
                        Parser.problem <| "Cannot parse to int: " ++ str
            )


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
