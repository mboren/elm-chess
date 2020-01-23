module History exposing (..)

import Piece exposing (Piece)
import Player exposing (Player)
import Ply exposing (Ply)
import Square exposing (Square)


type alias History =
    { pastMoves : List ( Ply, Ply )
    , latestPly : Maybe Ply
    }


empty =
    { pastMoves = [], latestPly = Nothing }


moveNumber : History -> Int
moveNumber history =
    1 + List.length history.pastMoves


add : Ply -> History -> History
add ply history =
    case history.latestPly of
        Nothing ->
            { history | latestPly = Just ply }

        Just p ->
            { history | pastMoves = ( p, ply ) :: history.pastMoves, latestPly = Nothing }


getTakenPieces : Player -> History -> List Piece
getTakenPieces player history =
    getPlayerMoves (Player.otherPlayer player) history
        |> List.filterMap Ply.getTakenPiece


getLastPly : History -> Maybe Ply
getLastPly history =
    case history.latestPly of
        Just p ->
            Just p

        Nothing ->
            case history.pastMoves of
                [] ->
                    Nothing

                ( w, b ) :: _ ->
                    Just b


toList : History -> List Ply
toList history =
    case history.latestPly of
        Nothing ->
            List.concatMap (\( w, b ) -> [ w, b ]) (List.reverse history.pastMoves)

        Just pl ->
            List.concatMap (\( w, b ) -> [ w, b ]) (List.reverse history.pastMoves) ++ [ pl ]


getPlayerMoves : Player -> History -> List Ply
getPlayerMoves player history =
    toList history |> List.filter (\ply -> Ply.getPlayer ply == player)


getPieceMoves : Piece.Piece -> History -> List Ply
getPieceMoves piece history =
    toList history |> List.filter (\ply -> Ply.getPiece ply == piece)


hasKingMoved : Player -> History -> Bool
hasKingMoved player history =
    not (List.isEmpty (getPieceMoves (Piece Piece.King player) history))


hasCaptureHappenedOnSquare : Square -> History -> Bool
hasCaptureHappenedOnSquare square history =
    toList history
        |> List.filter (\p->(Ply.getTakenPiece p) /= Nothing)
        |> List.filterMap Ply.getEnd
        |> List.any ((==) square)


hasQueensideRookMoved : Player -> History -> Bool
hasQueensideRookMoved player history =
    let
        startingSquare =
            Square (Player.firstRank player) 0
    in
    getPieceMoves (Piece Piece.Rook player) history
        |> List.filter (\ply -> startingSquare == Ply.getStart ply)
        |> List.isEmpty
        |> not


hasKingsideRookMoved : Player -> History -> Bool
hasKingsideRookMoved player history =
    let
        startingSquare =
            Square (Player.firstRank player) 7
    in
    getPieceMoves (Piece Piece.Rook player) history
        |> List.filter (\ply -> startingSquare == Ply.getStart ply)
        |> List.isEmpty
        |> not


toStrings :
    History
    -> List String -- TODO i dont like the name of this, how it works, or what it does
toStrings history =
    if history == empty then
        []

    else
        let
            formattedPastMoves =
                List.map (\( w, b ) -> [ Ply.toString w, Ply.toString b ]) history.pastMoves
        in
        (case history.latestPly of
            Nothing ->
                formattedPastMoves

            Just pl ->
                [ Ply.toString pl ] :: formattedPastMoves
        )
            |> List.reverse
            |> List.indexedMap (\i ss -> String.fromInt (i + 1) ++ ". " ++ String.join " " ss)
