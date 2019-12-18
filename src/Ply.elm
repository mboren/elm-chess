module Ply exposing (..)

import Piece exposing (Piece)
import Player exposing (Player)
import Square exposing (Square)


type alias StandardMoveData =
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
    = StandardMove StandardMoveData
    | QueensideCastle Player
    | KingsideCastle Player



--= Ply {start:Square, end:Square} | QueensideCastle Player | KingsideCastle Player | Promotion {start:Square, end:Square, promotion:Piece} | EnPassant {start:Square,end:Square}


getPlayer : Ply -> Player
getPlayer ply =
    case ply of
        StandardMove data ->
            data.player

        QueensideCastle player ->
            player

        KingsideCastle player ->
            player


getPiece : Ply -> Piece
getPiece ply =
    case ply of
        StandardMove data ->
            data.piece

        QueensideCastle player ->
            Piece Piece.King player

        KingsideCastle player ->
            Piece Piece.King player


getStart : Ply -> Square
getStart ply =
    case ply of
        StandardMove data ->
            data.start

        QueensideCastle player ->
            { rank = Player.lastRank player, file = 0 }

        KingsideCastle player ->
            { rank = Player.lastRank player, file = 7 }


getEnd : Ply -> Maybe Square
getEnd ply =
    case ply of
        StandardMove data ->
            Just data.end

        QueensideCastle player ->
            Nothing

        KingsideCastle player ->
            Nothing


getPieceKind : Ply -> Piece.PieceKind
getPieceKind ply =
    case ply of
        StandardMove data ->
            data.piece.kind

        QueensideCastle _ ->
            Piece.King

        KingsideCastle _ ->
            Piece.King


toString : Ply -> String
toString move =
    case move of
        StandardMove data ->
            let
                pieceString =
                    case data.piece.kind of
                        Piece.Pawn ->
                            case data.takes of
                                Nothing ->
                                    ""

                                Just _ ->
                                    Square.fileToString data.start.file

                        _ ->
                            Piece.pieceKindToString data.piece.kind |> String.toUpper

                takesString =
                    case data.takes of
                        Nothing ->
                            ""

                        Just _ ->
                            "x"

                destinationString =
                    Square.toString data.end

                promotionString =
                    Maybe.map Piece.toString data.promotion |> Maybe.withDefault ""
            in
            pieceString ++ takesString ++ destinationString ++ promotionString

        QueensideCastle _ ->
            "0-0-0"

        KingsideCastle _ ->
            "0-0"
