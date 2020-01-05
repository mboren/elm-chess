module TestPgnParsing exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Expect exposing (Expectation)
import Piece exposing (Piece, PieceKind(..))
import Player exposing (Player(..))
import Ply exposing (Ply(..))
import Position exposing (Position)
import Square exposing (Square)
import Test exposing (..)


basicPly : Square -> Square -> Piece.PieceKind -> Player -> Ply
basicPly start end pieceKind player =
    Ply.StandardMove { start = start, end = end, piece = Piece pieceKind player, player = player, takes = Nothing, promotion = Nothing }


ruyLopezWithWhiteKingsideCastlePosition : Position
ruyLopezWithWhiteKingsideCastlePosition =
    { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = WhitePlayer, kind = Rook }, Nothing, Just { color = WhitePlayer, kind = Bishop }, Just { color = WhitePlayer, kind = Queen }, Nothing, Just { color = WhitePlayer, kind = Rook }, Just { color = WhitePlayer, kind = King }, Nothing ], Array.fromList [ Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Nothing, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn } ], Array.fromList [ Nothing, Just { color = WhitePlayer, kind = Bishop }, Just { color = WhitePlayer, kind = Knight }, Nothing, Nothing, Just { color = WhitePlayer, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = WhitePlayer, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = BlackPlayer, kind = Pawn }, Nothing, Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = BlackPlayer, kind = Pawn }, Nothing, Just { color = BlackPlayer, kind = Knight }, Nothing, Nothing, Just { color = BlackPlayer, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Just { color = BlackPlayer, kind = Pawn }, Nothing, Nothing, Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn } ], Array.fromList [ Just { color = BlackPlayer, kind = Rook }, Nothing, Just { color = BlackPlayer, kind = Bishop }, Just { color = BlackPlayer, kind = Queen }, Just { color = BlackPlayer, kind = King }, Just { color = BlackPlayer, kind = Bishop }, Nothing, Just { color = BlackPlayer, kind = Rook } ] ] }, history = { latestPly = Just (KingsideCastle WhitePlayer), pastMoves = [ ( StandardMove { end = { file = 2, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 1, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 3, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 3, rank = 6 }, takes = Nothing } ), ( StandardMove { end = { file = 1, rank = 2 }, piece = { color = WhitePlayer, kind = Bishop }, player = WhitePlayer, promotion = Nothing, start = { file = 0, rank = 3 }, takes = Nothing }, StandardMove { end = { file = 5, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 0, rank = 3 }, piece = { color = WhitePlayer, kind = Bishop }, player = WhitePlayer, promotion = Nothing, start = { file = 1, rank = 4 }, takes = Nothing }, StandardMove { end = { file = 1, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 1, rank = 6 }, takes = Nothing } ), ( StandardMove { end = { file = 1, rank = 4 }, piece = { color = WhitePlayer, kind = Bishop }, player = WhitePlayer, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 0, rank = 5 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 0, rank = 6 }, takes = Nothing } ), ( StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 2, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = BlackPlayer }


suite : Test
suite =
    describe "PGN parsing"
        [ describe "fromPgn"
            [ test "Initial position" <|
                \_ ->
                    Expect.equal (Position.fromPgn "") (Ok Position.initial)
            , test "garbage" <|
                \_ ->
                    Expect.err (Position.fromPgn "asdfasdf")
            , test "Single ply" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 4) (Square 3 4) Pawn WhitePlayer)
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Position.fromPgn "1. e4") expectedPosition
            , test "Single unambiguous knight move without start coordinates" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 6) (Square 2 5) Piece.Knight WhitePlayer)
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Position.fromPgn "1. Nf3") expectedPosition
            , test "Single unambiguous knight move with start coordinates" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 6) (Square 2 5) Piece.Knight WhitePlayer)
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Position.fromPgn "1. Ng1f3") expectedPosition
            , test "Two plies" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 4) (Square 3 4) Pawn WhitePlayer)
                                |> Maybe.andThen (\x -> Position.makeMove x (basicPly (Square 6 4) (Square 4 4) Pawn Player.BlackPlayer))
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Position.fromPgn "1. e4 e5") expectedPosition
            , test "Three plies" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 4) (Square 3 4) Pawn WhitePlayer)
                                |> Maybe.andThen (\x -> Position.makeMove x (basicPly (Square 6 4) (Square 4 4) Pawn Player.BlackPlayer))
                                |> Maybe.andThen (\x -> Position.makeMove x (basicPly (Square 1 5) (Square 4 1) Bishop WhitePlayer))
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Position.fromPgn "1. e4 e5 2. Bb5") expectedPosition
            , test "Single invalid ply" <|
                \_ ->
                    Expect.err (Position.fromPgn "1. e5")
            , test "Ambiguous knight move without start pos should error" <|
                \_ ->
                    Expect.err (Position.fromPgn "1. Nf3 a6 2. Nc3 a5 3. Nd4 a4 4. Nb5")
            , test "Ambiguous knight move with start pos should succeed" <|
                \_ ->
                    Expect.ok (Position.fromPgn "1. Nf3 a6 2. Nc3 a5 3. Nd4 a4 4. Nc3b5")
            , test "Ruy lopez with white kingside castle" <|
                \_ ->
                    Expect.equal (Position.fromPgn "1. e4 e5 2. Ng1f3 Nb8c6 3. Bb5 a6 4. Ba4 b5 5. Bb3 Ng8f6 6. Nb1c3 d5 7. O-O") (Ok ruyLopezWithWhiteKingsideCastlePosition)
            ]
        ]
