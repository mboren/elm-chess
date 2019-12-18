module Player exposing (..)

-- TODO this type probably belongs in some other module


type Player
    = WhitePlayer
    | BlackPlayer


otherPlayer player =
    case player of
        WhitePlayer ->
            BlackPlayer

        BlackPlayer ->
            WhitePlayer


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
