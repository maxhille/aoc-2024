port module Worker exposing (main)

import Platform
import Shared exposing (Request, Response, puzzles, toResponse)
import Task
import Time


port fromUi : (Request -> msg) -> Sub msg


port toUi : Response -> Cmd msg


type Step
    = Process Request
    | Deliver Response


run : Request -> ( Int, Result String Int )
run { part, day, input } =
    puzzles
        |> List.drop (day - 1)
        |> List.head
        |> (\maybePuzzle ->
                case maybePuzzle of
                    Just puzzle ->
                        case part of
                            1 ->
                                puzzle.calculatePart1 input

                            2 ->
                                puzzle.calculatePart2 input

                            _ ->
                                Err "part was not 1 or 2"

                    Nothing ->
                        Err "worker could not find puzzle"
           )
        |> Tuple.pair part


main : Program () () Step
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \step _ ->
                case step of
                    Deliver response ->
                        ( (), toUi response )

                    Process request ->
                        ( ()
                        , Time.now
                            |> Task.andThen (\started -> Task.succeed ( started, run request ))
                            |> Task.andThen
                                (\( started, ( part, result ) ) ->
                                    Task.map (\stopped -> toResponse part (Time.posixToMillis stopped - Time.posixToMillis started) result) Time.now
                                )
                            |> Task.perform Deliver
                        )
        , subscriptions = \_ -> fromUi Process
        }
