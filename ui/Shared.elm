module Shared exposing (Request, Response, puzzles, toResponse, toResult)

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Puzzle exposing (Puzzle)


puzzles : List Puzzle
puzzles =
    [ Day01.puzzle
    , Day02.puzzle
    , Day03.puzzle
    , Day04.puzzle
    , Day05.puzzle
    , Day06.puzzle
    , Day07.puzzle
    , Day08.puzzle
    ]


type alias Request =
    { day : Int
    , part : Int
    , input : String
    }


type alias Response =
    { success : Bool
    , value : Maybe Int
    , error : Maybe String
    , part : Int
    , time : Int
    }


toResult : Response -> ( Int, Int, Result String Int )
toResult response =
    if response.success then
        ( response.part, response.time, Ok <| Maybe.withDefault -1 <| response.value )

    else
        ( response.part, response.time, Err <| Maybe.withDefault "(no error message in response)" <| response.error )


toResponse : Int -> Int -> Result String Int -> Response
toResponse part time result =
    case result of
        Ok value ->
            { part = part, time = time, success = True, value = Just value, error = Nothing }

        Err error ->
            { part = part, time = time, success = False, value = Nothing, error = Just error }
