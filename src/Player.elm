module Player exposing (..)


type Player
    = White
    | Black


direction : Player -> Int
direction player =
    case player of
        White ->
            1

        Black ->
            -1


otherPlayer player =
    case player of
        White ->
            Black

        Black ->
            White


toString player =
    case player of
        White ->
            "White"

        Black ->
            "Black"


lastRank player =
    case player of
        White ->
            7

        Black ->
            0


firstRank player =
    case player of
        White ->
            0

        Black ->
            7
