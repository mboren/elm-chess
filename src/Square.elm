module Square exposing (..)

import EverySet


type alias File =
    Int


type alias Rank =
    Int


ranks =
    [ 0, 1, 2, 3, 4, 5, 6, 7 ]


files =
    [ 0, 1, 2, 3, 4, 5, 6, 7 ]


type alias Square =
    { rank : Rank
    , file : File
    }


offset : Square -> ( Rank, File ) -> Maybe Square
offset square ( dr, df ) =
    let
        newSquare =
            { square | rank = square.rank + dr, file = square.file + df }
    in
    if newSquare.rank >= 0 && newSquare.rank <= 7 && newSquare.file >= 0 && newSquare.file <= 7 then
        Just newSquare

    else
        Nothing


allSquares =
    List.range 0 7
        |> List.map (\r -> List.map (\f -> Square r f) (List.range 0 7))
        |> List.concat
        |> EverySet.fromList


toString { rank, file } =
    fileToString file ++ rankToString rank


fileToString file =
    case file of
        0 ->
            "a"

        1 ->
            "b"

        2 ->
            "c"

        3 ->
            "d"

        4 ->
            "e"

        5 ->
            "f"

        6 ->
            "g"

        7 ->
            "h"

        _ ->
            "?"


rankToString rank =
    String.fromInt (rank + 1)
