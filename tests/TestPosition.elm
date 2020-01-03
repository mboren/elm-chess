module TestPosition exposing (..)

import EverySet
import Expect exposing (Expectation)
import Piece exposing (Piece)
import Player
import Ply
import Position
import Square exposing (Square)
import Test exposing (..)


enPassantPossiblePosition =
    let
        plies =
            [ Ply.StandardMove { start = Square 1 4, end = Square 3 4, player = Player.WhitePlayer, piece = Piece Piece.Pawn Player.WhitePlayer, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 0, end = Square 5 0, player = Player.BlackPlayer, piece = Piece Piece.Pawn Player.BlackPlayer, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 3 4, end = Square 4 4, player = Player.WhitePlayer, piece = Piece Piece.Pawn Player.WhitePlayer, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 5, end = Square 4 5, player = Player.BlackPlayer, piece = Piece Piece.Pawn Player.BlackPlayer, takes = Nothing, promotion = Nothing }
            ]
    in
    List.foldl
        (\ply pos ->
            case pos of
                Nothing ->
                    Nothing

                Just p ->
                    Position.makeMove p ply
        )
        (Just Position.initial)
        plies
        |> Maybe.withDefault Position.initial


enPassantPossiblePositionErrorCase =
    let
        plies =
            [ Ply.StandardMove { start = Square 1 4, end = Square 3 4, player = Player.WhitePlayer, piece = Piece Piece.Pawn Player.WhitePlayer, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 0, end = Square 4 0, player = Player.BlackPlayer, piece = Piece Piece.Pawn Player.BlackPlayer, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 3 4, end = Square 4 4, player = Player.WhitePlayer, piece = Piece Piece.Pawn Player.WhitePlayer, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 5, end = Square 4 5, player = Player.BlackPlayer, piece = Piece Piece.Pawn Player.BlackPlayer, takes = Nothing, promotion = Nothing }
            , Ply.EnPassant { end = { file = 5, rank = 5 }, player = Player.WhitePlayer, start = { file = 4, rank = 4 }, takenPawn = { file = 5, rank = 4 } }
            , Ply.StandardMove { start = Square 4 0, end = Square 3 0, player = Player.BlackPlayer, piece = Piece Piece.Pawn Player.BlackPlayer, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 1 1, end = Square 3 1, player = Player.WhitePlayer, piece = Piece Piece.Pawn Player.WhitePlayer, takes = Nothing, promotion = Nothing }
            ]
    in
    List.foldl
        (\ply pos ->
            case pos of
                Nothing ->
                    Nothing

                Just p ->
                    Position.makeMove p ply
        )
        (Just Position.initial)
        plies
        |> Maybe.withDefault Position.initial


suite : Test
suite =
    describe "Position module"
        [ describe "Position.getPossiblePawnMoves"
            [ test "Initial position" <|
                \_ ->
                    let
                        start =
                            Square 1 0

                        makePly end =
                            Ply.StandardMove { player = Player.WhitePlayer, piece = Piece Piece.Pawn Player.WhitePlayer, start = start, end = end, takes = Nothing, promotion = Nothing }

                        expectedMoves =
                            EverySet.fromList [ makePly (Square 2 0), makePly (Square 3 0) ]
                    in
                    Position.getPossiblePawnMoves Player.WhitePlayer start Position.initial
                        |> Expect.equal expectedMoves
            , test "En Passant error case" <|
                \_ ->
                    let
                        enPassantPly =
                            Ply.EnPassant
                                { end = { file = 1, rank = 2 }
                                , player = Player.BlackPlayer
                                , start = { file = 0, rank = 3 }
                                , takenPawn = { file = 1, rank = 3 }
                                }
                    in
                    Expect.true "The correct en passant ply is in the set" (EverySet.member enPassantPly (Position.getPossiblePawnMoves Player.BlackPlayer (Square 3 0) enPassantPossiblePositionErrorCase))
            , test "En Passant" <|
                \_ ->
                    let
                        enPassantPly =
                            Ply.EnPassant
                                { end = { file = 5, rank = 5 }
                                , player = Player.WhitePlayer
                                , start = { file = 4, rank = 4 }
                                , takenPawn = { file = 5, rank = 4 }
                                }
                    in
                    Expect.true "The correct en passant ply is in the set" (EverySet.member enPassantPly (Position.getPossiblePawnMoves Player.WhitePlayer (Square 4 4) enPassantPossiblePosition))
            ]
        ]
