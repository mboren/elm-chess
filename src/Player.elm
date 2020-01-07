module Player exposing (..)

-- TODO this type probably belongs in some other module


type Player
    = WhitePlayer
    | BlackPlayer


direction : Player -> Int
direction player =
    case player of
        WhitePlayer ->
            1

        BlackPlayer ->
            -1


otherPlayer player =
    case player of
        WhitePlayer ->
            BlackPlayer

        BlackPlayer ->
            WhitePlayer


toString player =
    case player of
        WhitePlayer ->
            "White"

        BlackPlayer ->
            "Black"


lastRank player =
    case player of
        WhitePlayer ->
            7

        BlackPlayer ->
            0


firstRank player =
    case player of
        WhitePlayer ->
            0

        BlackPlayer ->
            7
