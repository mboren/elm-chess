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
            [ Ply.StandardMove { start = Square 1 4, end = Square 3 4, player = Player.White, piece = Piece Piece.Pawn Player.White, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 0, end = Square 5 0, player = Player.Black, piece = Piece Piece.Pawn Player.Black, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 3 4, end = Square 4 4, player = Player.White, piece = Piece Piece.Pawn Player.White, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 5, end = Square 4 5, player = Player.Black, piece = Piece Piece.Pawn Player.Black, takes = Nothing, promotion = Nothing }
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
            [ Ply.StandardMove { start = Square 1 4, end = Square 3 4, player = Player.White, piece = Piece Piece.Pawn Player.White, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 0, end = Square 4 0, player = Player.Black, piece = Piece Piece.Pawn Player.Black, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 3 4, end = Square 4 4, player = Player.White, piece = Piece Piece.Pawn Player.White, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 6 5, end = Square 4 5, player = Player.Black, piece = Piece Piece.Pawn Player.Black, takes = Nothing, promotion = Nothing }
            , Ply.EnPassant { end = { file = 5, rank = 5 }, player = Player.White, start = { file = 4, rank = 4 }, takenPawn = { file = 5, rank = 4 } }
            , Ply.StandardMove { start = Square 4 0, end = Square 3 0, player = Player.Black, piece = Piece Piece.Pawn Player.Black, takes = Nothing, promotion = Nothing }
            , Ply.StandardMove { start = Square 1 1, end = Square 3 1, player = Player.White, piece = Piece Piece.Pawn Player.White, takes = Nothing, promotion = Nothing }
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
                            Ply.StandardMove { player = Player.White, piece = Piece Piece.Pawn Player.White, start = start, end = end, takes = Nothing, promotion = Nothing }

                        expectedMoves =
                            EverySet.fromList [ makePly (Square 2 0), makePly (Square 3 0) ]
                    in
                    Position.getPossiblePawnMoves Player.White start Position.initial
                        |> Expect.equal expectedMoves
            , test "En Passant error case" <|
                \_ ->
                    let
                        enPassantPly =
                            Ply.EnPassant
                                { end = { file = 1, rank = 2 }
                                , player = Player.Black
                                , start = { file = 0, rank = 3 }
                                , takenPawn = { file = 1, rank = 3 }
                                }
                    in
                    Expect.true "The correct en passant ply is in the set" (EverySet.member enPassantPly (Position.getPossiblePawnMoves Player.Black (Square 3 0) enPassantPossiblePositionErrorCase))
            , test "En Passant" <|
                \_ ->
                    let
                        enPassantPly =
                            Ply.EnPassant
                                { end = { file = 5, rank = 5 }
                                , player = Player.White
                                , start = { file = 4, rank = 4 }
                                , takenPawn = { file = 5, rank = 4 }
                                }
                    in
                    Expect.true "The correct en passant ply is in the set" (EverySet.member enPassantPly (Position.getPossiblePawnMoves Player.White (Square 4 4) enPassantPossiblePosition))
            ]
        , describe "Position.toPgn"
            [ test "ambiguous knight move" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = White, kind = Rook },Nothing,Just { color = White, kind = Bishop },Just { color = White, kind = Queen },Just { color = White, kind = King },Just { color = White, kind = Bishop },Nothing,Just { color = White, kind = Rook }],Array.fromList [Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Just { color = White, kind = Knight },Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Just { color = White, kind = Knight },Nothing,Nothing,Just { color = Black, kind = Knight },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Just { color = Black, kind = Knight },Nothing,Nothing],Array.fromList [Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn }],Array.fromList [Just { color = Black, kind = Rook },Nothing,Just { color = Black, kind = Bishop },Just { color = Black, kind = Queen },Just { color = Black, kind = King },Just { color = Black, kind = Bishop },Nothing,Just { color = Black, kind = Rook }]] }, history = { latestPly = Just (StandardMove { end = { file = 1, rank = 4 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 2, rank = 2 }, takes = Nothing }), pastMoves = [(StandardMove { end = { file = 3, rank = 3 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 5, rank = 2 }, takes = Nothing },StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 2, rank = 5 }, takes = Nothing }),(StandardMove { end = { file = 2, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 1, rank = 0 }, takes = Nothing },StandardMove { end = { file = 2, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing }),(StandardMove { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing },StandardMove { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing })] }, playerToMove = Black }
                    in
                    Expect.equal "1. Nf3 Nf6 2. Nc3 Nc6 3. Nd4 Ne5 4. Ncb5" (Position.toPgn position)
            , test "single pawn move" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = White, kind = Rook },Just { color = White, kind = Knight },Just { color = White, kind = Bishop },Just { color = White, kind = Queen },Just { color = White, kind = King },Just { color = White, kind = Bishop },Just { color = White, kind = Knight },Just { color = White, kind = Rook }],Array.fromList [Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Nothing,Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = White, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn }],Array.fromList [Just { color = Black, kind = Rook },Just { color = Black, kind = Knight },Just { color = Black, kind = Bishop },Just { color = Black, kind = Queen },Just { color = Black, kind = King },Just { color = Black, kind = Bishop },Just { color = Black, kind = Knight },Just { color = Black, kind = Rook }]] }, history = { latestPly = Just (StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }), pastMoves = [] }, playerToMove = Black }
                    in
                    Expect.equal "1. e4" (Position.toPgn position)
            , test "two plies" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = White, kind = Rook },Just { color = White, kind = Knight },Just { color = White, kind = Bishop },Just { color = White, kind = Queen },Just { color = White, kind = King },Just { color = White, kind = Bishop },Just { color = White, kind = Knight },Just { color = White, kind = Rook }],Array.fromList [Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Nothing,Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = White, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = Black, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Nothing,Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn }],Array.fromList [Just { color = Black, kind = Rook },Just { color = Black, kind = Knight },Just { color = Black, kind = Bishop },Just { color = Black, kind = Queen },Just { color = Black, kind = King },Just { color = Black, kind = Bishop },Just { color = Black, kind = Knight },Just { color = Black, kind = Rook }]] }, history = { latestPly = Nothing, pastMoves = [(StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing },StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing })] }, playerToMove = White }
                    in
                    Expect.equal "1. e4 e5" (Position.toPgn position)
            , test "three plies" <|
                \_ ->
                    let
                        position = { board = { columns = 8, data = Array.fromList [Array.fromList [Just { color = White, kind = Rook },Just { color = White, kind = Knight },Just { color = White, kind = Bishop },Just { color = White, kind = Queen },Just { color = White, kind = King },Just { color = White, kind = Bishop },Nothing,Just { color = White, kind = Rook }],Array.fromList [Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Nothing,Just { color = White, kind = Pawn },Just { color = White, kind = Pawn },Just { color = White, kind = Pawn }],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Just { color = White, kind = Knight },Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = White, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Just { color = Black, kind = Pawn },Nothing,Nothing,Nothing],Array.fromList [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],Array.fromList [Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Nothing,Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn },Just { color = Black, kind = Pawn }],Array.fromList [Just { color = Black, kind = Rook },Just { color = Black, kind = Knight },Just { color = Black, kind = Bishop },Just { color = Black, kind = Queen },Just { color = Black, kind = King },Just { color = Black, kind = Bishop },Just { color = Black, kind = Knight },Just { color = Black, kind = Rook }]] }, history = { latestPly = Just (StandardMove { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }), pastMoves = [(StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing },StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing })] }, playerToMove = Black }
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
                        positionBeforeCastling = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Nothing, Just { color = White, kind = Rook }, Just { color = White, kind = King }, Nothing ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = Black, kind = Bishop }, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = White, kind = Bishop }, Nothing, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Nothing, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (KingsideCastle White), pastMoves = [ ( StandardMove { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 1, rank = 3 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 1, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = Black }
                    in
                    Expect.ok (Position.isPlyValid (Ply.KingsideCastle Player.Black) positionBeforeCastling)
            ]
        , describe "makeMove"
            [ test "ply with wrong turn should return Nothing" <|
                \_ ->
                    Expect.equal Nothing (Position.makeMove Position.initial (Ply.StandardMove { start = Square 6 0, end = Square 5 0, player = Player.Black, piece = Piece Piece.Pawn Player.Black, takes = Nothing, promotion = Nothing }))
            , test "illegal ply by current player should be allowed" <|
                \_ ->
                    Expect.equal Nothing (Position.makeMove Position.initial (Ply.StandardMove { start = Square 6 0, end = Square 5 0, player = Player.Black, piece = Piece Piece.Pawn Player.Black, takes = Nothing, promotion = Nothing }))
            ]
        ]
