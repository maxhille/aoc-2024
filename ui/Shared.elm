module Shared exposing (Request, Response, puzzles, toResponse, toResult)

import Day01
import Day02
import Puzzle exposing (Puzzle)


puzzles : List Puzzle
puzzles =
    [ Day01.puzzle
    , Day02.puzzle
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
    }


toResult : Response -> ( Int, Result String Int )
toResult response =
    if response.success then
        ( response.part, Ok <| Maybe.withDefault -1 <| response.value )

    else
        ( response.part, Err <| Maybe.withDefault "(no error message in response)" <| response.error )


toResponse : Int -> Result String Int -> Response
toResponse part result =
    case result of
        Ok value ->
            { part = part, success = True, value = Just value, error = Nothing }

        Err error ->
            { part = part, success = False, value = Nothing, error = Just error }
