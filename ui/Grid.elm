module Grid exposing
    ( Grid
    , Pos
    , find
    , fromLists
    , get
    , initialize
    , positionedFold
    , set
    , size
    , toPositionedList
    , toStrings
    )

import Array exposing (Array)


type alias Pos =
    ( Int, Int )


type alias Grid a =
    Array (Array a)


initialize : ( Int, Int ) -> (Pos -> a) -> Grid a
initialize ( xs, ys ) f =
    Array.initialize ys (\y -> Array.initialize xs (\x -> f ( x, y )))


toStrings : (a -> String) -> Grid a -> List String
toStrings f grid =
    Array.map (\xs -> Array.map f xs |> Array.toList |> String.concat) grid |> Array.toList


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


set : ( Int, Int ) -> a -> Grid a -> Grid a
set ( x, y ) a grid =
    case Array.get y grid of
        Nothing ->
            grid

        Just xs ->
            Array.set y (Array.set x a xs) grid


find : (a -> Bool) -> Grid a -> List ( Int, Int )
find fn =
    findYHelp fn 0 []


findYHelp : (a -> Bool) -> Int -> List ( Int, Int ) -> Grid a -> List ( Int, Int )
findYHelp fn y hits grid =
    case Array.get y grid of
        Nothing ->
            hits

        Just xs ->
            findYHelp fn (y + 1) (findXHelp fn ( 0, y ) hits xs) grid


findXHelp : (a -> Bool) -> ( Int, Int ) -> List ( Int, Int ) -> Array a -> List ( Int, Int )
findXHelp fn ( x, y ) hits xs =
    case Array.get x xs of
        Nothing ->
            hits

        Just a ->
            findXHelp fn
                ( x + 1, y )
                (if fn a then
                    ( x, y ) :: hits

                 else
                    hits
                )
                xs


toPositionedList : Grid a -> List ( ( Int, Int ), a )
toPositionedList =
    Array.toIndexedList
        >> List.map (\( y, xs ) -> Array.toIndexedList xs |> List.map (\( x, a ) -> ( ( x, y ), a )))
        >> List.concat


positionedFold : (( Pos, a ) -> b -> b) -> b -> Grid a -> b
positionedFold fn b =
    toPositionedList >> List.foldl fn b
