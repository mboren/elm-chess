module Ply exposing (..)

import EverySet
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
    | EnPassant EnPassantData



--= Ply {start:Square, end:Square} | QueensideCastle Player | KingsideCastle Player | Promotion {start:Square, end:Square, promotion:Piece} | EnPassant {start:Square,end:Square}


getPlayer : Ply -> Player
getPlayer ply =
    case ply of
        StandardMove data ->
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
        StandardMove data ->
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
        StandardMove data ->
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
        StandardMove data ->
            data.start

        EnPassant data ->
            data.start

        QueensideCastle player ->
            { rank = Player.lastRank player, file = 4 }

        KingsideCastle player ->
            { rank = Player.lastRank player, file = 4 }


getEnd : Ply -> Maybe Square
getEnd ply =
    case ply of
        StandardMove data ->
            Just data.end

        EnPassant data ->
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

        EnPassant _ ->
            Piece.Pawn

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

                context =
                    {- With rooks and knights there can be situations where just giving the piece name and end
                    square is ambiguous. For proper notation, we would only provide the minimum amount of information
                    necessary to resolve ambiguity (in order of preference: Nothing, only rank, only file, both rank and
                    file), but for now i'm just going to add both every time.
                    -}
                    case data.piece.kind of
                        Piece.Rook ->
                            Square.toString data.start
                        Piece.Knight ->
                            Square.toString data.start
                        _ -> ""


                takesString =
                    case data.takes of
                        Nothing ->
                            ""

                        Just _ ->
                            "x"

                destinationString =
                    Square.toString data.end

                promotionString =
                    Maybe.map (.kind >> Piece.pieceKindToString >> String.toUpper >> (\x -> "=" ++ x)) data.promotion
                        |> Maybe.withDefault ""
            in
            pieceString ++ context ++ takesString ++ destinationString ++ promotionString

        EnPassant data ->
            -- TODO this isn't proper notation
            [ Square.fileToString data.start.file, "x", Square.toString data.takenPawn, "e.p." ]
                |> String.join ""

        QueensideCastle _ ->
            "0-0-0"

        KingsideCastle _ ->
            "0-0"


toThreat : Ply -> Maybe Square
toThreat ply =
    case ply of
        StandardMove data ->
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
        StandardMove data ->
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
            (\( sq, ply ) ->
                if sq == square then
                    True

                else
                    False
            )
        |> (\l ->
                case l of
                    [] ->
                        Nothing

                    ( sq, ply ) :: [] ->
                        Just ply

                    h :: t ->
                        Nothing
           )
