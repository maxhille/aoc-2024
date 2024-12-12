module Puzzle exposing
    ( Puzzle
    , cartesian
    , charParser
    , gridParser
    , inputParser
    , listParser
    , notImplemented
    , singleDigitParser
    )

import Grid exposing (Grid)
import Parser exposing ((|.), (|=), Parser)


type alias Puzzle =
    { validate : String -> Result String String
    , calculatePart1 : String -> Result String Int
    , calculatePart2 : String -> Result String Int
    }


notImplemented : a -> Result String b
notImplemented =
    \_ -> Err "Not implemented"


inputParser : Parser a -> Parser (List a)
inputParser lineParser =
    Parser.loop [] (inputParserHelp lineParser)


inputParserHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
inputParserHelp lineParser revLines =
    Parser.oneOf
        [ Parser.succeed (Parser.Done (List.reverse revLines))
            |. Parser.end
        , Parser.succeed (Parser.Loop revLines)
            |. Parser.symbol "\n"
        , Parser.succeed (\line -> Parser.Loop (line :: revLines))
            |= lineParser
        ]


listParser : { take : List (Parser a), skip : List (Parser b) } -> Parser (List a)
listParser itemParser =
    Parser.loop [] (listParserHelp itemParser)


listParserHelp : { take : List (Parser a), skip : List (Parser b) } -> List a -> Parser (Parser.Step (List a) (List a))
listParserHelp itemParser revItems =
    Parser.oneOf
        [ Parser.succeed (\item -> Parser.Loop (item :: revItems))
            |= Parser.oneOf itemParser.take
        , Parser.succeed (Parser.Loop revItems)
            |. Parser.oneOf itemParser.skip
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
        ]


singleDigitParser : Parser Int
singleDigitParser =
    (Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isDigit
    )
        |> Parser.andThen
            (\str ->
                case String.toInt str of
                    Just int ->
                        Parser.succeed int

                    Nothing ->
                        Parser.problem <| "Cannot parse to int: " ++ str
            )


gridParser : Parser a -> Parser (Grid a)
gridParser itemParser =
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
                , item = itemParser
                , trailing = Parser.Optional
                }
        , trailing = Parser.Optional
        }
        |> Parser.map Grid.fromLists


charParser : (Char -> Bool) -> Parser Char
charParser isChar =
    Parser.getChompedString (Parser.chompIf isChar)
        |> Parser.andThen
            (\str ->
                case str |> String.toList >> List.head of
                    Nothing ->
                        Parser.problem "could not parse char"

                    Just char ->
                        Parser.succeed char
            )


cartesian : List a -> List b -> List ( a, b )
cartesian as_ bs =
    List.concatMap (\x -> List.map (\y -> ( x, y )) bs) as_
