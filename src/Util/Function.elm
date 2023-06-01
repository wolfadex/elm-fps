module Util.Function exposing (applyIf)


applyIf : Bool -> (a -> a) -> a -> a
applyIf condition f x =
    if condition then
        f x

    else
        x
