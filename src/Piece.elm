module Piece exposing (..)

import Player exposing (Player(..))


type PieceKind
    = Pawn
    | Rook
    | Knight
    | Bishop
    | King
    | Queen


type alias Piece =
    { kind : PieceKind
    , color : Player
    }


fromString : String -> Maybe Piece
fromString s =
    case s of
        "P" ->
            Just (Piece Pawn White)

        "R" ->
            Just (Piece Rook White)

        "N" ->
            Just (Piece Knight White)

        "B" ->
            Just (Piece Bishop White)

        "K" ->
            Just (Piece King White)

        "Q" ->
            Just (Piece Queen White)

        "p" ->
            Just (Piece Pawn Black)

        "r" ->
            Just (Piece Rook Black)

        "n" ->
            Just (Piece Knight Black)

        "b" ->
            Just (Piece Bishop Black)

        "k" ->
            Just (Piece King Black)

        "q" ->
            Just (Piece Queen Black)

        _ ->
            Nothing


pieceKindToString pk =
    case pk of
        Pawn ->
            "p"

        Rook ->
            "r"

        Knight ->
            "n"

        Bishop ->
            "b"

        King ->
            "k"

        Queen ->
            "q"


toString p =
    case p.color of
        White ->
            case p.kind of
                Pawn ->
                    "♙"

                Rook ->
                    "♖"

                Knight ->
                    "♘"

                Bishop ->
                    "♗"

                King ->
                    "♔"

                Queen ->
                    "♕"

        Black ->
            case p.kind of
                Pawn ->
                    "♟"

                Rook ->
                    "♜"

                Knight ->
                    "♞"

                Bishop ->
                    "♝"

                King ->
                    "♚"

                Queen ->
                    "♛"

value pieceKind =
    case pieceKind of
        Pawn -> 1
        Rook -> 5
        Knight -> 3
        Bishop -> 3
        King -> 0
        Queen -> 50
