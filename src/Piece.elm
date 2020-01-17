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
            Just (Piece Pawn WhitePlayer)

        "R" ->
            Just (Piece Rook WhitePlayer)

        "N" ->
            Just (Piece Knight WhitePlayer)

        "B" ->
            Just (Piece Bishop WhitePlayer)

        "K" ->
            Just (Piece King WhitePlayer)

        "Q" ->
            Just (Piece Queen WhitePlayer)

        "p" ->
            Just (Piece Pawn BlackPlayer)

        "r" ->
            Just (Piece Rook BlackPlayer)

        "n" ->
            Just (Piece Knight BlackPlayer)

        "b" ->
            Just (Piece Bishop BlackPlayer)

        "k" ->
            Just (Piece King BlackPlayer)

        "q" ->
            Just (Piece Queen BlackPlayer)

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
        WhitePlayer ->
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

        BlackPlayer ->
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
