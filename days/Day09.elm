module Day09 exposing (calculatePart1, calculatePart2, parser, puzzle)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Puzzle exposing (Puzzle)


type alias Diskmap =
    List Int


type Block
    = Free
    | File Int


type alias Span =
    { length : Int
    , type_ : Block
    }


calculatePart1 : Diskmap -> Result String Int
calculatePart1 =
    diskmapToBlocks >> defragBlocks >> checksum >> Ok


defragBlocks : Array Block -> Array Block
defragBlocks array =
    defragBlocksHelp 0 (Array.length array - 1) array


defragBlocksHelp : Int -> Int -> Array Block -> Array Block
defragBlocksHelp n m array =
    if n >= m then
        array

    else
        case ( Array.get n array, Array.get m array ) of
            ( Just Free, Just Free ) ->
                defragBlocksHelp n (m - 1) array

            ( Just _, Just Free ) ->
                defragBlocksHelp (n + 1) m array

            ( Just Free, Just file ) ->
                defragBlocksHelp (n + 1) (m - 1) (array |> Array.set n file |> Array.set m Free)

            ( Just _, Just _ ) ->
                defragBlocksHelp (n + 1) m array

            ( _, _ ) ->
                array


diskmapToBlocks : Diskmap -> Array Block
diskmapToBlocks =
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


calculatePart2 : Diskmap -> Result String Int
calculatePart2 =
    toSpans
        >> defragSpans
        >> spansToBlocks
        >> checksum
        >> Ok


defragSpans : Array Span -> Array Span
defragSpans array =
    defragSpansHelp (Array.length array - 1) array


defragSpansHelp : Int -> Array Span -> Array Span
defragSpansHelp m array =
    if m == 0 then
        array

    else
        case Array.get m array of
            Nothing ->
                array

            Just span2 ->
                if span2.type_ == Free then
                    defragSpansHelp (m - 1) array

                else
                    case findFree 0 m span2 array of
                        Nothing ->
                            defragSpansHelp (m - 1) array

                        Just { index, span } ->
                            let
                                start =
                                    Array.slice 0 index array

                                ( mid, newM ) =
                                    if span.length == span2.length then
                                        ( Array.fromList <| [ span2 ], m )

                                    else
                                        ( Array.fromList <| [ span2, { length = span.length - span2.length, type_ = Free } ], m + 1 )

                                end =
                                    Array.slice (index + 1) (Array.length array) array

                                newArray =
                                    end
                                        |> Array.append mid
                                        |> Array.append start
                                        |> Array.set newM { length = span2.length, type_ = Free }
                            in
                            defragSpansHelp (newM - 1) newArray


findFree : Int -> Int -> Span -> Array Span -> Maybe { index : Int, span : Span }
findFree n m span array =
    if n >= m then
        Nothing

    else
        case Array.get n array of
            Just possibleFree ->
                if possibleFree.type_ == Free && possibleFree.length >= span.length then
                    Just { index = n, span = possibleFree }

                else
                    findFree (n + 1) m span array

            -- name the case _ instead of Nothing to get rid of the warning that we should use Maybe.map/andThen which breaks TCO
            _ ->
                Nothing


spansToBlocks : Array Span -> Array Block
spansToBlocks =
    Array.foldl (\span blocks -> Array.append blocks (Array.repeat span.length span.type_)) Array.empty


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


toSpans : Diskmap -> Array Span
toSpans =
    List.foldl
        (\int { id, free, spans } ->
            if free then
                { id = id
                , free = False
                , spans =
                    if int == 0 then
                        spans

                    else
                        { length = int, type_ = Free } :: spans
                }

            else
                { id = id + 1
                , free = True
                , spans =
                    if int == 0 then
                        spans

                    else
                        { length = int, type_ = File id } :: spans
                }
        )
        { id = 0
        , free = False
        , spans = []
        }
        >> .spans
        >> List.reverse
        >> Array.fromList


parser : Parser Diskmap
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , spaces = Parser.chompWhile (always False)
        , item = Puzzle.singleDigitParser
        , trailing = Parser.Optional
        }


puzzle : Puzzle
puzzle =
    { validate = Parser.run parser >> Result.map (\_ -> "could not parse") >> Result.mapError Parser.deadEndsToString
    , calculatePart1 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart1
    , calculatePart2 = Parser.run parser >> Result.mapError Parser.deadEndsToString >> Result.andThen calculatePart2
    }
