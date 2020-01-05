module TestPgnParsing exposing (..)

import Expect exposing (Expectation)
import Piece exposing (Piece, PieceKind(..))
import Player exposing (Player(..))
import Ply exposing (Ply(..))
import Position
import Square exposing (Square)
import Test exposing (..)


basicPly : Square -> Square -> Piece.PieceKind -> Player -> Ply
basicPly start end pieceKind player =
    Ply.StandardMove { start = start, end = end, piece = Piece pieceKind player, player = player, takes = Nothing, promotion = Nothing }


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
            ]
        ]
