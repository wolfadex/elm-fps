module Array2d exposing
    ( Array2d
    , fromList
    , get
    )

import Array exposing (Array)


type alias Array2d a =
    Array (Array a)


fromList : List (List a) -> Array2d a
fromList list =
    list
        |> Array.fromList
        |> Array.map Array.fromList


get : Int -> Int -> Array2d a -> Maybe a
get x y array =
    case Array.get y array of
        Nothing ->
            Nothing

        Just row ->
            Array.get x row
