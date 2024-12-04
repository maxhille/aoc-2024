module Grid exposing
    ( Grid
    , fromLists
    , get
    , size
    )

import Array exposing (Array)


type alias Grid a =
    Array (Array a)


fromLists : List (List a) -> Grid a
fromLists =
    List.map Array.fromList >> Array.fromList


size : Grid a -> { xs : Int, ys : Int }
size grid =
    { ys = Array.length grid
    , xs =
        grid
            |> Array.map Array.length
            |> Array.toList
            |> List.maximum
            |> Maybe.withDefault 0
    }


get : ( Int, Int ) -> Grid a -> Maybe a
get ( x, y ) =
    Array.get y >> Maybe.andThen (Array.get x)
