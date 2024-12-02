module Puzzle exposing (Puzzle, inputParser, listParser)

import Parser exposing ((|.), (|=), Parser)


type alias Puzzle =
    { validate : String -> Result String String
    , calculatePart1 : String -> Result String Int
    , calculatePart2 : String -> Result String Int
    }


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
