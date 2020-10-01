module Ply exposing (..)

import Piece exposing (Piece)
import Player exposing (Player)
import Square exposing (Square)


type alias StandardPlyData =
    { player : Player
    , piece : Piece
    , start : Square
    , end : Square
    , takes : Maybe Piece
    , promotion : Maybe Piece
    }


type alias EnPassantData =
    { player : Player
    , start : Square
    , end : Square
    , takenPawn : Square
    }


type Ply
    = Standard StandardPlyData
    | QueensideCastle Player
    | KingsideCastle Player
    | EnPassant EnPassantData


getPlayer : Ply -> Player
getPlayer ply =
    case ply of
        Standard data ->
            data.player

        EnPassant data ->
            data.player

        QueensideCastle player ->
            player

        KingsideCastle player ->
            player


getTakenPiece : Ply -> Maybe Piece
getTakenPiece ply =
    case ply of
        Standard data ->
            data.takes

        EnPassant data ->
            Just (Piece Piece.Pawn (Player.otherPlayer data.player))

        KingsideCastle _ ->
            Nothing

        QueensideCastle _ ->
            Nothing


getPiece : Ply -> Piece
getPiece ply =
    case ply of
        Standard data ->
            data.piece

        EnPassant data ->
            Piece Piece.Pawn data.player

        QueensideCastle player ->
            Piece Piece.King player

        KingsideCastle player ->
            Piece Piece.King player


getStart : Ply -> Square
getStart ply =
    case ply of
        Standard data ->
            data.start

        EnPassant data ->
            data.start

        QueensideCastle player ->
            { rank = Player.firstRank player, file = 4 }

        KingsideCastle player ->
            { rank = Player.firstRank player, file = 4 }


getEnd : Ply -> Square
getEnd ply =
    case ply of
        Standard data ->
            data.end

        EnPassant data ->
            data.end

        QueensideCastle player ->
            { rank = Player.firstRank player, file = 2 }

        KingsideCastle player ->
            { rank = Player.firstRank player, file = 6 }


toThreat : Ply -> Maybe Square
toThreat ply =
    case ply of
        Standard data ->
            Just data.end

        EnPassant data ->
            Just data.takenPawn

        QueensideCastle _ ->
            Nothing

        KingsideCastle _ ->
            Nothing


toSquareForMoveSelection : Ply -> Square
toSquareForMoveSelection ply =
    case ply of
        Standard data ->
            data.end

        EnPassant data ->
            data.end

        QueensideCastle player ->
            Square (Player.firstRank player) 2

        KingsideCastle player ->
            Square (Player.firstRank player) 6


getMoveAssociatedWithSquare : List Ply -> Square -> Maybe Ply
getMoveAssociatedWithSquare plies square =
    plies
        |> List.map (\ply -> ( toSquareForMoveSelection ply, ply ))
        |> List.filter
            (\( sq, ply ) -> sq == square)
        |> (\l ->
                case l of
                    [] ->
                        Nothing

                    ( sq, ply ) :: [] ->
                        Just ply

                    h :: t ->
                        Nothing
           )
