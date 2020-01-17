module TestPosition exposing (..)

import Array
import EverySet
import Expect exposing (Expectation)
import Piece exposing (Piece, PieceKind(..))
import Player exposing (Player(..))
import Ply exposing (Ply(..))
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
        , describe "Position.toPgn"
            [ test "ambiguous knight move" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = WhitePlayer, kind = Rook },Nothing,Just { color = WhitePlayer, kind = Bishop },Just { color = WhitePlayer, kind = Queen },Just { color = WhitePlayer, kind = King },Just { color = WhitePlayer, kind = Bishop },Nothing,Just { color = WhitePlayer, kind = Rook }],Array.fromList [Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Just { color = WhitePlayer, kind = Knight },Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Just { color = WhitePlayer, kind = Knight },Nothing,Nothing,Just { color = BlackPlayer, kind = Knight },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Just { color = BlackPlayer, kind = Knight },Nothing,Nothing],Array.fromList [Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn }],Array.fromList [Just { color = BlackPlayer, kind = Rook },Nothing,Just { color = BlackPlayer, kind = Bishop },Just { color = BlackPlayer, kind = Queen },Just { color = BlackPlayer, kind = King },Just { color = BlackPlayer, kind = Bishop },Nothing,Just { color = BlackPlayer, kind = Rook }]] }, history = { latestPly = Just (StandardMove { end = { file = 1, rank = 4 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 2, rank = 2 }, takes = Nothing }), pastMoves = [(StandardMove { end = { file = 3, rank = 3 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 5, rank = 2 }, takes = Nothing },StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 2, rank = 5 }, takes = Nothing }),(StandardMove { end = { file = 2, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 1, rank = 0 }, takes = Nothing },StandardMove { end = { file = 2, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing }),(StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing },StandardMove { end = { file = 5, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing })] }, playerToMove = BlackPlayer }
                    in
                    Expect.equal "1. Nf3 Nf6 2. Nc3 Nc6 3. Nd4 Ne5 4. Ncb5" (Position.toPgn position)
            , test "single pawn move" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = WhitePlayer, kind = Rook },Just { color = WhitePlayer, kind = Knight },Just { color = WhitePlayer, kind = Bishop },Just { color = WhitePlayer, kind = Queen },Just { color = WhitePlayer, kind = King },Just { color = WhitePlayer, kind = Bishop },Just { color = WhitePlayer, kind = Knight },Just { color = WhitePlayer, kind = Rook }],Array.fromList [Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Nothing,Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = WhitePlayer, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn }],Array.fromList [Just { color = BlackPlayer, kind = Rook },Just { color = BlackPlayer, kind = Knight },Just { color = BlackPlayer, kind = Bishop },Just { color = BlackPlayer, kind = Queen },Just { color = BlackPlayer, kind = King },Just { color = BlackPlayer, kind = Bishop },Just { color = BlackPlayer, kind = Knight },Just { color = BlackPlayer, kind = Rook }]] }, history = { latestPly = Just (StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }), pastMoves = [] }, playerToMove = BlackPlayer }
                    in
                    Expect.equal "1. e4" (Position.toPgn position)
            , test "two plies" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = WhitePlayer, kind = Rook },Just { color = WhitePlayer, kind = Knight },Just { color = WhitePlayer, kind = Bishop },Just { color = WhitePlayer, kind = Queen },Just { color = WhitePlayer, kind = King },Just { color = WhitePlayer, kind = Bishop },Just { color = WhitePlayer, kind = Knight },Just { color = WhitePlayer, kind = Rook }],Array.fromList [Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Nothing,Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = WhitePlayer, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = BlackPlayer, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Nothing,Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn }],Array.fromList [Just { color = BlackPlayer, kind = Rook },Just { color = BlackPlayer, kind = Knight },Just { color = BlackPlayer, kind = Bishop },Just { color = BlackPlayer, kind = Queen },Just { color = BlackPlayer, kind = King },Just { color = BlackPlayer, kind = Bishop },Just { color = BlackPlayer, kind = Knight },Just { color = BlackPlayer, kind = Rook }]] }, history = { latestPly = Nothing, pastMoves = [(StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing },StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing })] }, playerToMove = WhitePlayer }
                    in
                    Expect.equal "1. e4 e5" (Position.toPgn position)
            , test "three plies" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = WhitePlayer, kind = Rook },Just { color = WhitePlayer, kind = Knight },Just { color = WhitePlayer, kind = Bishop },Just { color = WhitePlayer, kind = Queen },Just { color = WhitePlayer, kind = King },Just { color = WhitePlayer, kind = Bishop },Nothing,Just { color = WhitePlayer, kind = Rook }],Array.fromList [Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Nothing,Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn },Just { color = WhitePlayer, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Just { color = WhitePlayer, kind = Knight },Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = WhitePlayer, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = BlackPlayer, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Nothing,Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn },Just { color = BlackPlayer, kind = Pawn }],Array.fromList [Just { color = BlackPlayer, kind = Rook },Just { color = BlackPlayer, kind = Knight },Just { color = BlackPlayer, kind = Bishop },Just { color = BlackPlayer, kind = Queen },Just { color = BlackPlayer, kind = King },Just { color = BlackPlayer, kind = Bishop },Just { color = BlackPlayer, kind = Knight },Just { color = BlackPlayer, kind = Rook }]] }, history = { latestPly = Just (StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }), pastMoves = [(StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing },StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing })] }, playerToMove = BlackPlayer }
                    in
                    Expect.equal "1. e4 e5 2. Nf3" (Position.toPgn position)

            ]
        , describe "canPieceMoveBetweenSquares"
            [ test "pawn from initial" <|
                \_ ->
                    Expect.true "Expected pawn to be able to move a2->a3" (Position.canPieceMoveBetweenSquares Position.initial (Square 1 0) (Square 2 0))
            ]
        , describe "isPlyValid"
            [ test "black castling" <|
                \_ ->
                    let
                        positionBeforeCastling = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = WhitePlayer, kind = Rook }, Just { color = WhitePlayer, kind = Knight }, Just { color = WhitePlayer, kind = Bishop }, Just { color = WhitePlayer, kind = Queen }, Nothing, Just { color = WhitePlayer, kind = Rook }, Just { color = WhitePlayer, kind = King }, Nothing ], Array.fromList [ Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Nothing, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn }, Just { color = WhitePlayer, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = WhitePlayer, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = BlackPlayer, kind = Bishop }, Nothing, Nothing, Just { color = WhitePlayer, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = WhitePlayer, kind = Bishop }, Nothing, Nothing, Just { color = BlackPlayer, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = BlackPlayer, kind = Knight }, Nothing, Nothing ], Array.fromList [ Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn }, Nothing, Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn }, Just { color = BlackPlayer, kind = Pawn } ], Array.fromList [ Just { color = BlackPlayer, kind = Rook }, Just { color = BlackPlayer, kind = Knight }, Just { color = BlackPlayer, kind = Bishop }, Just { color = BlackPlayer, kind = Queen }, Just { color = BlackPlayer, kind = King }, Nothing, Nothing, Just { color = BlackPlayer, kind = Rook } ] ] }, history = { latestPly = Just (KingsideCastle WhitePlayer), pastMoves = [ ( StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 1, rank = 3 }, piece = { color = BlackPlayer, kind = Bishop }, player = BlackPlayer, promotion = Nothing, start = { file = 5, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 1, rank = 4 }, piece = { color = WhitePlayer, kind = Bishop }, player = WhitePlayer, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 5, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = BlackPlayer }
                    in
                    Expect.ok (Position.isPlyValid (Ply.KingsideCastle Player.BlackPlayer) positionBeforeCastling)
            ]
        ]
