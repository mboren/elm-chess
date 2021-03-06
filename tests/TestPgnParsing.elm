module TestPgnParsing exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Parser
import Pgn
import Piece exposing (Piece, PieceKind(..))
import Player exposing (Player(..))
import Ply exposing (Ply(..))
import Position exposing (Position)
import Square exposing (Square)
import Test exposing (..)


fuzzSquare : Fuzzer Square
fuzzSquare =
    Fuzz.map2 Square (Fuzz.intRange 0 7) (Fuzz.intRange 0 7)


fuzzKnightDelta =
    Fuzz.oneOf
        [ Fuzz.constant ( -2, -1 ), Fuzz.constant ( -2, 1 ), Fuzz.constant ( -1, -2 ), Fuzz.constant ( -1, 2 ), Fuzz.constant ( 1, -2 ), Fuzz.constant ( 1, 2 ), Fuzz.constant ( 2, -1 ), Fuzz.constant ( 2, 1 ) ]


fuzzKnightMove =
    Fuzz.map2 (\start offset -> ( start, Square.offset start offset )) fuzzSquare fuzzKnightDelta


fuzzKnightMoveString =
    Fuzz.map
        (\( start, maybeEnd ) ->
            case maybeEnd of
                Nothing ->
                    "Ng1xf3"

                Just e ->
                    "N" ++ Square.toString start ++ "x" ++ Square.toString e
        )
        fuzzKnightMove


basicPly : Square -> Square -> Piece.PieceKind -> Player -> Ply
basicPly start end pieceKind player =
    Ply.Standard { start = start, end = end, piece = Piece pieceKind player, player = player, takes = Nothing, promotion = Nothing }


ruyLopezWithWhiteKingsideCastlePosition : Position
ruyLopezWithWhiteKingsideCastlePosition =
    { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Nothing, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Nothing, Just { color = White, kind = Rook }, Just { color = White, kind = King }, Nothing ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Nothing, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (KingsideCastle White), pastMoves = [ ( Standard { end = { file = 2, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 1, rank = 0 }, takes = Nothing }, Standard { end = { file = 3, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 3, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 1, rank = 2 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 0, rank = 3 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 0, rank = 3 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 1, rank = 4 }, takes = Nothing }, Standard { end = { file = 1, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 1, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, Standard { end = { file = 0, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 0, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, Standard { end = { file = 2, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = Black }


positionWithNoWhiteAPawn : Position
positionWithNoWhiteAPawn =
    { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Nothing, pastMoves = [ ( Standard { end = { file = 1, rank = 4 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 0, rank = 3 }, takes = Just { color = Black, kind = Pawn } }, Standard { end = { file = 0, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 0, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 0, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 0, rank = 1 }, takes = Nothing }, Standard { end = { file = 1, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 6 }, takes = Nothing } ) ] }, playerToMove = White }


kingPawnOpening : Position
kingPawnOpening =
    { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }), pastMoves = [] }, playerToMove = Black }


enPassant : Position
enPassant =
    { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Just { color = Black, kind = Knight }, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Nothing, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (EnPassant { end = { file = 3, rank = 5 }, player = White, start = { file = 4, rank = 4 }, takenPawn = { file = 3, rank = 4 } }), pastMoves = [ ( Standard { end = { file = 4, rank = 4 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 3 }, takes = Nothing }, Standard { end = { file = 3, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 3, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 2, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing } ) ] }, playerToMove = Black }


pawnCapture : Position
pawnCapture =
    { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Nothing, pastMoves = [ ( Standard { end = { file = 3, rank = 5 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 3, rank = 4 }, takes = Nothing }, Standard { end = { file = 3, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 2, rank = 6 }, takes = Just { color = White, kind = Pawn } } ), ( Standard { end = { file = 3, rank = 4 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 3 }, takes = Just { color = Black, kind = Pawn } }, Standard { end = { file = 4, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 3, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 3, rank = 6 }, takes = Nothing } ) ] }, playerToMove = White }


ambiguousQueenMove : Position
ambiguousQueenMove =
    { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Nothing, Nothing, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Bishop }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing ], Array.fromList [ Just { color = White, kind = Knight }, Nothing, Just { color = Black, kind = Queen }, Nothing, Just { color = Black, kind = Queen }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Nothing, pastMoves = [ ( Standard { end = { file = 0, rank = 4 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 2, rank = 5 }, takes = Nothing }, Standard { end = { file = 2, rank = 4 }, piece = { color = Black, kind = Queen }, player = Black, promotion = Nothing, start = { file = 2, rank = 0 }, takes = Nothing } ), ( Standard { end = { file = 2, rank = 5 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 1, rank = 3 }, takes = Nothing }, Standard { end = { file = 2, rank = 0 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Just { color = Black, kind = Queen }, start = { file = 2, rank = 1 }, takes = Nothing } ), ( Standard { end = { file = 1, rank = 3 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 3, rank = 4 }, takes = Nothing }, Standard { end = { file = 2, rank = 1 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 2 }, takes = Just { color = White, kind = Pawn } } ), ( Standard { end = { file = 3, rank = 4 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 2, rank = 2 }, takes = Nothing }, Standard { end = { file = 1, rank = 2 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 3 }, takes = Nothing } ), ( Standard { end = { file = 2, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 1, rank = 0 }, takes = Nothing }, Standard { end = { file = 1, rank = 3 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 4 }, takes = Nothing } ), ( Standard { end = { file = 2, rank = 7 }, piece = { color = White, kind = Pawn }, player = White, promotion = Just { color = White, kind = Queen }, start = { file = 2, rank = 6 }, takes = Nothing }, Standard { end = { file = 2, rank = 7 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 4 }, takes = Just { color = White, kind = Queen } } ), ( Standard { end = { file = 2, rank = 6 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 2, rank = 5 }, takes = Nothing }, Standard { end = { file = 1, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 5 }, takes = Nothing } ), ( EnPassant { end = { file = 2, rank = 5 }, player = White, start = { file = 3, rank = 4 }, takenPawn = { file = 2, rank = 4 } }, Standard { end = { file = 1, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 2 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 3, rank = 1 }, takes = Nothing }, Standard { end = { file = 2, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 2, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 3, rank = 1 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 4, rank = 2 }, takes = Nothing }, Standard { end = { file = 5, rank = 4 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 4, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 2 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 3, rank = 1 }, takes = Nothing }, Standard { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Queen }, player = Black, promotion = Nothing, start = { file = 3, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 3, rank = 1 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 4, rank = 2 }, takes = Nothing }, Standard { end = { file = 3, rank = 5 }, piece = { color = Black, kind = Queen }, player = Black, promotion = Nothing, start = { file = 3, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 2 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 2, rank = 4 }, takes = Nothing }, Standard { end = { file = 6, rank = 3 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 7, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 2, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 3, rank = 5 }, takes = Nothing }, Standard { end = { file = 7, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 3, rank = 5 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 5, rank = 3 }, takes = Just { color = Black, kind = Pawn } }, Standard { end = { file = 4, rank = 5 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 4 }, takes = Nothing } ), ( Standard { end = { file = 5, rank = 3 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 2, rank = 0 }, takes = Nothing }, Standard { end = { file = 5, rank = 4 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 4, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 3, rank = 4 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 3, rank = 3 }, takes = Nothing }, Standard { end = { file = 4, rank = 5 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 2, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 3, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 3, rank = 1 }, takes = Nothing }, Standard { end = { file = 3, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 3, rank = 6 }, takes = Nothing } ) ] }, playerToMove = White }


autoTestCases =
    [ { position = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Nothing, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Nothing, pastMoves = [ ( Standard { end = { file = 4, rank = 4 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 5, rank = 2 }, takes = Just { color = Black, kind = Pawn } }, Standard { end = { file = 1, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, Standard { end = { file = 1, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 1, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = White }
      , text = "1. e4 e5 2. Ng1f3 b6 3. Nf3xe5 b5"
      }
    , { position = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Nothing, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (Standard { end = { file = 4, rank = 4 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 5, rank = 2 }, takes = Just { color = Black, kind = Pawn } }), pastMoves = [ ( Standard { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, Standard { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = Black }
      , text = "1. Ng1f3 e5 2. Nf3xe5"
      }
    , { position = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Nothing, pastMoves = [ ( Standard { end = { file = 5, rank = 2 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 5, rank = 1 }, takes = Nothing }, Standard { end = { file = 4, rank = 3 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 5, rank = 5 }, takes = Just { color = White, kind = Pawn } } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ) ] }, playerToMove = White }
      , text = "1. e4 Ng8f6 2. f3 Nf6xe4"
      }
    , { position = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Nothing, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Just { color = White, kind = Knight } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (Standard { end = { file = 7, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Just { color = Black, kind = Pawn } }), pastMoves = [ ( Standard { end = { file = 4, rank = 2 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 7, rank = 2 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 7, rank = 3 }, takes = Nothing } ), ( Standard { end = { file = 5, rank = 5 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 5, rank = 4 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Just { color = White, kind = Pawn } } ), ( Standard { end = { file = 5, rank = 4 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 5, rank = 3 }, takes = Nothing }, Standard { end = { file = 7, rank = 3 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 7, rank = 4 }, takes = Nothing } ), ( Standard { end = { file = 5, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 5, rank = 1 }, takes = Nothing }, Standard { end = { file = 7, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 7, rank = 6 }, takes = Nothing } ) ] }, playerToMove = Black }
      , text = "1. f4 h5 2. f5 h4 3. f6 Ng8xf6 4. e3 h3 5. Ng1xh3"
      }
    , { position = ambiguousQueenMove
      , text = "1. d4 d6 2. d5 Be6 3. Bf4 Bf5 4. Bxd6 Be6 5. Bc5 Ng8h6 6. Be3 Nh6g4 7. Bd2 Qd6 8. Be3 Qe5 9. Bd2 Bf5 10. Be3 c5 11. dxc6 b6 12. c7 b5 13. c8=Q Bxc8 14. Nb1c3 b4 15. Nc3d5 b3 16. Nd5b4 bxc2 17. Nb4c6 c1=Q 18. Nc6a5 Qc1c5"
      }
    , { position = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = King }, Nothing ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Nothing ], Array.fromList [ Just { color = White, kind = Bishop }, Nothing, Nothing, Just { color = White, kind = Queen }, Just { color = White, kind = Rook }, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Just { color = Black, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Nothing, Just { color = Black, kind = Queen }, Nothing, Just { color = Black, kind = Rook }, Just { color = Black, kind = King }, Nothing ] ] }, history = { latestPly = Nothing, pastMoves = [ ( Standard { end = { file = 6, rank = 2 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 7, rank = 3 }, takes = Nothing }, KingsideCastle Black ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Rook }, player = White, promotion = Nothing, start = { file = 4, rank = 0 }, takes = Just { color = Black, kind = Bishop } }, Standard { end = { file = 4, rank = 6 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 7, rank = 3 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 6, rank = 4 }, takes = Nothing }, Standard { end = { file = 0, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 0, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 6, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 2, rank = 0 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 5, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 3, rank = 5 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 4 }, takes = Just { color = Black, kind = Pawn } }, Standard { end = { file = 4, rank = 3 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 4 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 4 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 3 }, takes = Nothing }, Standard { end = { file = 5, rank = 4 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 7, rank = 2 }, takes = Nothing } ), ( Standard { end = { file = 3, rank = 3 }, piece = { color = White, kind = Queen }, player = White, promotion = Nothing, start = { file = 3, rank = 0 }, takes = Just { color = Black, kind = Pawn } }, Standard { end = { file = 7, rank = 2 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 2, rank = 7 }, takes = Just { color = White, kind = Pawn } } ), ( Standard { end = { file = 7, rank = 2 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 6, rank = 1 }, takes = Just { color = Black, kind = Knight } }, Standard { end = { file = 3, rank = 3 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 4 }, takes = Just { color = White, kind = Pawn } } ), ( Standard { end = { file = 3, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 3, rank = 1 }, takes = Nothing }, Standard { end = { file = 7, rank = 2 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 5, rank = 3 }, takes = Just { color = White, kind = Pawn } } ), ( Standard { end = { file = 4, rank = 0 }, piece = { color = White, kind = Rook }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, Standard { end = { file = 5, rank = 3 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 7, rank = 4 }, takes = Nothing } ), ( KingsideCastle White, Standard { end = { file = 7, rank = 4 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 5, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 7, rank = 2 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 7, rank = 1 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 3 }, takes = Nothing } ), ( Standard { end = { file = 2, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 1, rank = 0 }, takes = Nothing }, Standard { end = { file = 6, rank = 3 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 5, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 0, rank = 3 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 1, rank = 4 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 1, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, Standard { end = { file = 2, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 2, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, Standard { end = { file = 3, rank = 5 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 3, rank = 6 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = White }

      -- clicking ai move after this position caused a huge amount of branches i thought would never be hit to be hit.
      -- The test always passed, but i used to get crazy console output from it. That's fixed now though.
      , text = "1. e4 e5 2. Nf3 d6 3. Bb5 c6 4. Ba4 Nf6 5. Nc3 Ng4 6. h3 Nf6 7. O-O Nh5 8. Re1 Nf4 9. d4 Nxh3 10. gxh3 exd4 11. Qxd4 Bxh3 12. e5 Bf5 13. exd6 Be4 14. Bg5 f6 15. Bh4 a5 16. Rxe4 Be7 17. Bg3 O-O"
      }
    , { position = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = Rook }, Nothing, Just { color = White, kind = King }, Nothing ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = White, kind = Bishop }, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Nothing, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Nothing, Just { color = Black, kind = Rook }, Just { color = Black, kind = King }, Nothing ] ] }, history = { latestPly = Nothing, pastMoves = [ ( Standard { end = { file = 7, rank = 2 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 7, rank = 1 }, takes = Nothing }, KingsideCastle Black ), ( Standard { end = { file = 4, rank = 0 }, piece = { color = White, kind = Rook }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, Standard { end = { file = 2, rank = 4 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 7 }, takes = Nothing } ), ( KingsideCastle White, Standard { end = { file = 3, rank = 3 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 2, rank = 5 }, takes = Nothing } ), ( Standard { end = { file = 1, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, Standard { end = { file = 2, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = White }
      , text = "1. e4 e5 2. Nf3 Nf6 3. Bb5 Nc6 4. O-O Nd4 5. Re1 Bc5 6. h3 O-O"
      }
    , { position = { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Nothing, Just { color = White, kind = Rook }, Just { color = White, kind = King }, Nothing ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = Black, kind = Bishop }, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = White, kind = Bishop }, Nothing, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Nothing, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (KingsideCastle White), pastMoves = [ ( Standard { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, Standard { end = { file = 1, rank = 3 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 1, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, Standard { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( Standard { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, Standard { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = Black }

      -- right before black castles
      , text = "1. e4 e5 2. Bb5 Nf6 3. Nf3 Bb4 4. O-O"
      }
    ]

suite : Test
suite =
    describe "PGN parsing"
        [ describe "putting it together"
            [ describe "ply" <|
                [ test "pawn advance a4" <|
                    \_ -> Expect.equal (Ok (Pgn.PawnAdvance (Square 3 0) Nothing)) (Parser.run Pgn.ply "a4")
                , test "pawn capture dxe4" <|
                    \_ -> Expect.equal (Ok (Pgn.PawnCapture { startFile = 3, end = Square 3 4, promotion = Nothing })) (Parser.run Pgn.ply "dxe4")
                , test "pawn capture dxe1=Q" <|
                    \_ -> Expect.equal (Ok (Pgn.PawnCapture { startFile = 3, end = Square 0 4, promotion = Just Piece.Queen })) (Parser.run Pgn.ply "dxe1=Q")
                , test "kingside castle" <|
                    \_ -> Expect.equal (Parser.run Pgn.ply "O-O") (Ok Pgn.KingsideCastle)
                , test "Nf3" <|
                    \_ -> Expect.equal (Ok (Pgn.Standard { pieceKind = Piece.Knight, startRank = Nothing, startFile = Nothing, end = Square 2 5 })) (Parser.run Pgn.ply "Nf3")
                , test "Nxf3" <|
                    \_ -> Expect.equal (Ok (Pgn.Capture { pieceKind = Piece.Knight, startRank = Nothing, startFile = Nothing, end = Square 2 5 })) (Parser.run Pgn.ply "Nxf3")
                , test "Ngxf3" <|
                    \_ -> Expect.equal (Ok (Pgn.Capture { pieceKind = Piece.Knight, startRank = Nothing, startFile = Just 6, end = Square 2 5 })) (Parser.run Pgn.ply "Ngxf3")
                , test "Ngf3" <|
                    \_ -> Expect.equal (Ok (Pgn.Standard { pieceKind = Piece.Knight, startRank = Nothing, startFile = Just 6, end = Square 2 5 })) (Parser.run Pgn.ply "Ngf3")
                , test "N1xf3" <|
                    \_ -> Expect.equal (Ok (Pgn.Capture { pieceKind = Piece.Knight, startRank = Just 0, startFile = Nothing, end = Square 2 5 })) (Parser.run Pgn.ply "N1xf3")
                , test "N1f3" <|
                    \_ -> Expect.equal (Ok (Pgn.Standard { pieceKind = Piece.Knight, startRank = Just 0, startFile = Nothing, end = Square 2 5 })) (Parser.run Pgn.ply "N1f3")
                , test "Ng1xf3" <|
                    \_ -> Expect.equal (Ok (Pgn.Capture { pieceKind = Piece.Knight, startRank = Just 0, startFile = Just 6, end = Square 2 5 })) (Parser.run Pgn.ply "Ng1xf3")
                , test "Nf3xe5" <|
                    \_ -> Expect.equal (Ok (Pgn.Capture { pieceKind = Piece.Knight, startRank = Just 2, startFile = Just 5, end = Square 4 4 })) (Parser.run Pgn.ply "Nf3xe5")
                , test "Ng1f3" <|
                    \_ -> Expect.equal (Ok (Pgn.Standard { pieceKind = Piece.Knight, startRank = Just 0, startFile = Just 6, end = Square 2 5 })) (Parser.run Pgn.ply "Ng1f3")
                , fuzz fuzzKnightMoveString "fuzz knight moves should parse" <|
                    \s ->
                        Expect.ok (Parser.run Pgn.ply s)
                , fuzz fuzzKnightMove "fuzz knight moves should parse correctly" <|
                    \( start, maybeEnd ) ->
                        case maybeEnd of
                            Nothing ->
                                Expect.equal 0 0

                            Just e ->
                                let
                                    moveText =
                                        "N" ++ Square.toString start ++ "x" ++ Square.toString e

                                    expectedPly =
                                        Pgn.Capture { pieceKind = Piece.Knight, startRank = Just start.rank, startFile = Just start.file, end = e }
                                in
                                Expect.equal (Ok expectedPly) (Parser.run Pgn.ply moveText)
                ]
            , describe "moves" <|
                [ test "Initial position" <|
                    \_ ->
                        Expect.equal (Ok []) (Parser.run Pgn.moves "")
                , test "move number followed by garbage" <|
                    \_ ->
                        Expect.err (Parser.run Pgn.moves "1. jjjjjj")
                , test "Just move number" <|
                    \_ ->
                        Expect.err (Parser.run Pgn.moves "1.")
                , test "garbage" <|
                    \_ ->
                        Expect.err (Parser.run Pgn.moves "jjjjjj")
                , test "Single ply" <|
                    \_ -> Expect.equal (Ok [ Pgn.PawnAdvance (Square 3 4) Nothing ]) (Parser.run Pgn.moves "1. e4")
                , test "Single unambiguous knight move without start coordinates" <|
                    \_ ->
                        Expect.equal (Ok [ Pgn.Standard { pieceKind = Piece.Knight, startRank = Nothing, startFile = Nothing, end = Square 2 5 } ]) (Parser.run Pgn.moves "1. Nf3")
                , test "Two plies" <|
                    \_ ->
                        let
                            expectedPosition =
                                [ Pgn.PawnAdvance (Square 3 4) Nothing
                                , Pgn.PawnAdvance (Square 4 4) Nothing
                                ]
                        in
                        Expect.equal (Ok expectedPosition) (Parser.run Pgn.moves "1. e4 e5")
                , test "Three plies" <|
                    \_ ->
                        let
                            expectedPosition =
                                [ Pgn.PawnAdvance (Square 3 4) Nothing
                                , Pgn.PawnAdvance (Square 4 4) Nothing
                                , Pgn.Standard { pieceKind = Piece.Bishop, startRank = Nothing, startFile = Nothing, end = Square 4 1 }
                                ]
                        in
                        Expect.equal (Ok expectedPosition) (Parser.run Pgn.moves "1. e4 e5 2. Bb5")
                , test "Four plies" <|
                    \_ ->
                        let
                            expectedPosition =
                                [ Pgn.PawnAdvance (Square 3 4) Nothing
                                , Pgn.PawnAdvance (Square 4 4) Nothing
                                , Pgn.Standard { pieceKind = Piece.Bishop, startRank = Nothing, startFile = Nothing, end = Square 4 1 }
                                , Pgn.PawnAdvance (Square 5 2) Nothing
                                ]
                        in
                        Expect.equal (Ok expectedPosition) (Parser.run Pgn.moves "1. e4 e5 2. Bb5 c6")

                , test "! should be ignored" <|
                    \_ ->
                        let
                            expectedPosition =
                                [ Pgn.PawnAdvance (Square 3 4) Nothing
                                , Pgn.PawnAdvance (Square 4 4) Nothing
                                , Pgn.Standard { pieceKind = Piece.Bishop, startRank = Nothing, startFile = Nothing, end = Square 4 1 }
                                , Pgn.PawnAdvance (Square 5 2) Nothing
                                ]
                        in
                        Expect.equal (Ok expectedPosition) (Parser.run Pgn.moves "1. e4 e5! 2. Bb5 c6")
                , test "? should be ignored" <|
                    \_ ->
                        let
                            expectedPosition =
                                [ Pgn.PawnAdvance (Square 3 4) Nothing
                                , Pgn.PawnAdvance (Square 4 4) Nothing
                                , Pgn.Standard { pieceKind = Piece.Bishop, startRank = Nothing, startFile = Nothing, end = Square 4 1 }
                                , Pgn.PawnAdvance (Square 5 2) Nothing
                                ]
                        in
                        Expect.equal (Ok expectedPosition) (Parser.run Pgn.moves "1. e4 e5 2. Bb5? c6")
                , test "+ should be ignored" <|
                    \_ ->
                        let
                            expectedPosition =
                                [ Pgn.PawnAdvance (Square 3 4) Nothing
                                , Pgn.PawnAdvance (Square 4 4) Nothing
                                , Pgn.Standard { pieceKind = Piece.Bishop, startRank = Nothing, startFile = Nothing, end = Square 4 1 }
                                , Pgn.PawnAdvance (Square 5 2) Nothing
                                ]
                        in
                        Expect.equal (Ok expectedPosition) (Parser.run Pgn.moves "1. e4 e5+ 2. Bb5 c6+")
                ]
            ]
        , describe "building blocks"
            [ describe "castle"
                [ test "kingside" <|
                    \_ -> Expect.equal (Parser.run Pgn.castle "O-O") (Ok Pgn.KingsideCastle)
                , test "queenside" <|
                    \_ -> Expect.equal (Parser.run Pgn.castle "O-O-O") (Ok Pgn.QueensideCastle)
                , test "garbage" <|
                    \_ -> Expect.err (Parser.run Pgn.castle "O-O-")
                , test "garbage 2" <|
                    \_ -> Expect.err (Parser.run Pgn.castle "O-")
                ]
            , describe "promotion suffix"
                [ test "=Q" <|
                    \_ ->
                        Expect.equal (Parser.run Pgn.promotion "=Q") (Ok (Just Piece.Queen))
                , test "=N" <|
                    \_ ->
                        Expect.equal (Parser.run Pgn.promotion "=N") (Ok (Just Piece.Knight))
                ]
            , describe "findPawnThatCanMoveToPosition"
                [ test "a4" <|
                    \_ ->
                        Expect.equal (Position.findPawnThatCanMoveToSquare (Square 3 0) Position.initial) (Just (Square 1 0))
                , test "a3" <|
                    \_ ->
                        Expect.equal (Position.findPawnThatCanMoveToSquare (Square 2 0) Position.initial) (Just (Square 1 0))
                , test "a6" <|
                    \_ ->
                        Expect.equal (Position.findPawnThatCanMoveToSquare (Square 5 0) kingPawnOpening) (Just (Square 6 0))
                , test "a5" <|
                    \_ ->
                        Expect.equal (Position.findPawnThatCanMoveToSquare (Square 4 0) kingPawnOpening) (Just (Square 6 0))
                , test "square too far from pawn" <|
                    \_ ->
                        Expect.equal (Position.findPawnThatCanMoveToSquare (Square 5 0) Position.initial) Nothing
                , test "file without pawn" <|
                    \_ ->
                        Expect.equal (Position.findPawnThatCanMoveToSquare (Square 3 0) positionWithNoWhiteAPawn) Nothing
                ]
            , describe "rank"
                [ test "valid rank" <|
                    \_ -> Expect.equal (Parser.run Pgn.rank "1") (Ok 0)
                , test "invalid rank 9" <|
                    \_ -> Expect.err (Parser.run Pgn.rank "9")
                , test "invalid rank -1" <|
                    \_ -> Expect.err (Parser.run Pgn.rank "-1")
                , test "invalid rank A" <|
                    \_ -> Expect.err (Parser.run Pgn.rank "A")
                ]
            , describe "moveNumber"
                [ test "valid moveNumber 1." <|
                    \_ -> Expect.equal (Parser.run Pgn.moveNumber "1.") (Ok ())
                , test "valid moveNumber 5." <|
                    \_ -> Expect.equal (Parser.run Pgn.moveNumber "5.") (Ok ())
                , test "multiple digits" <|
                    \_ -> Expect.equal (Parser.run Pgn.moveNumber "22.") (Ok ())
                , test "zero" <|
                    \_ -> Expect.err (Parser.run Pgn.moveNumber "0.")
                , test "Missing dot" <|
                    \_ -> Expect.err (Parser.run Pgn.moveNumber "1")
                ]
            , describe "file"
                [ test "valid file: a" <|
                    \_ -> Expect.equal (Parser.run Pgn.file "a") (Ok 0)
                , test "valid file: h" <|
                    \_ -> Expect.equal (Parser.run Pgn.file "h") (Ok 7)
                , test "invalid file: 1" <|
                    \_ -> Expect.err (Parser.run Pgn.file "1")
                , test "empty string" <|
                    \_ -> Expect.err (Parser.run Pgn.file "")
                ]
            , describe "square"
                [ test "valid square: a1" <|
                    \_ -> Expect.equal (Parser.run Pgn.square "a1") (Ok (Square 0 0))
                , test "valid square: h8" <|
                    \_ -> Expect.equal (Parser.run Pgn.square "h8") (Ok (Square 7 7))
                , test "invalid square: 1" <|
                    \_ -> Expect.err (Parser.run Pgn.square "1")
                , test "invalid square: a" <|
                    \_ -> Expect.err (Parser.run Pgn.square "a")
                , test "empty string" <|
                    \_ -> Expect.err (Parser.run Pgn.square "")
                ]
            , describe "piece kind"
                [ test "rook" <|
                    \_ -> Expect.equal (Parser.run Pgn.pieceKind "R") (Ok Piece.Rook)
                , test "knight" <|
                    \_ -> Expect.equal (Parser.run Pgn.pieceKind "N") (Ok Piece.Knight)
                , test "bishop" <|
                    \_ -> Expect.equal (Parser.run Pgn.pieceKind "B") (Ok Piece.Bishop)
                , test "king" <|
                    \_ -> Expect.equal (Parser.run Pgn.pieceKind "K") (Ok Piece.King)
                , test "queen" <|
                    \_ -> Expect.equal (Parser.run Pgn.pieceKind "Q") (Ok Piece.Queen)
                , test "lowercase" <|
                    \_ -> Expect.err (Parser.run Pgn.pieceKind "b")
                , test "symbol" <|
                    \_ -> Expect.err (Parser.run Pgn.pieceKind ".")
                , test "empty string" <|
                    \_ -> Expect.err (Parser.run Pgn.pieceKind "")
                ]
            ]
        , describe "fromPgn"
            [ test "Initial position" <|
                \_ ->
                    Expect.equal (Pgn.toPosition "") (Ok Position.initial)
            , test "garbage" <|
                \_ ->
                    Expect.err (Pgn.toPosition "asdfasdf")
            , test "Single ply" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 4) (Square 3 4) Pawn White)
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Pgn.toPosition "1. e4") expectedPosition
            , test "Single unambiguous knight move without start coordinates" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 0 6) (Square 2 5) Piece.Knight White)
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Pgn.toPosition "1. Nf3") expectedPosition
            , test "Single unambiguous knight move with start coordinates" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 0 6) (Square 2 5) Piece.Knight White)
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Pgn.toPosition "1. Ng1f3") expectedPosition
            , test "Two plies" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 4) (Square 3 4) Pawn White)
                                |> Maybe.andThen (\x -> Position.makeMove x (basicPly (Square 6 4) (Square 4 4) Pawn Player.Black))
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Pgn.toPosition "1. e4 e5") expectedPosition
            , test "Three plies" <|
                \_ ->
                    let
                        expectedPosition =
                            Position.makeMove Position.initial (basicPly (Square 1 4) (Square 3 4) Pawn White)
                                |> Maybe.andThen (\x -> Position.makeMove x (basicPly (Square 6 4) (Square 4 4) Pawn Player.Black))
                                |> Maybe.andThen (\x -> Position.makeMove x (basicPly (Square 0 5) (Square 4 1) Bishop White))
                                |> Result.fromMaybe ""
                    in
                    Expect.equal (Pgn.toPosition "1. e4 e5 2. Bb5") expectedPosition
            , test "Single invalid ply" <|
                \_ ->
                    Expect.err (Pgn.toPosition "1. e5")
            , test "Ambiguous knight move without start pos should error" <|
                \_ ->
                    Expect.err (Pgn.toPosition "1. Nf3 a6 2. Nc3 a5 3. Nd4 a4 4. Nb5")
            , test "Ambiguous queen move without start pos should error" <|
                \_ ->
                    Expect.err (Pgn.toPosition "1. d4 d6 2. d5 Be6 3. Bf4 Bf5 4. Bxd6 Be6 5. Bc5 Ng8h6 6. Be3 Nh6g4 7. Bd2 Qd6 8. Be3 Qe5 9. Bd2 Bf5 10. Be3 c5 11. dxc6 b6 12. c7 b5 13. c8=Q Bxc8 14. Nb1c3 b4 15. Nc3d5 b3 16. Nd5b4 bxc2 17. Nb4c6 c1=Q 18. Nc6a5 Qc5")
            , test "Ambiguous knight move with start pos should succeed" <|
                \_ ->
                    Expect.ok (Pgn.toPosition "1. Nf3 a6 2. Nc3 a5 3. Nd4 a4 4. Nc3b5")
            , test "Ruy lopez with white kingside castle" <|
                \_ ->
                    Expect.equal (Ok ruyLopezWithWhiteKingsideCastlePosition) (Pgn.toPosition "1. e4 e5 2. Ng1f3 Nb8c6 3. Bb5 a6 4. Ba4 b5 5. Bb3 Ng8f6 6. Nb1c3 d5 7. O-O")
            , test "Ruy lopez with invalid white queenside castle should fail" <|
                \_ ->
                    Expect.err (Pgn.toPosition "1. e4 e5 2. Ng1f3 Nb8c6 3. Bb5 a6 4. Ba4 b5 5. Bb3 Ng8f6 6. Nb1c3 d5 7. O-O-O")
            , test "Pawn captures on both sides" <|
                \_ ->
                    Expect.equal (Ok pawnCapture) (Pgn.toPosition "1. e4 d5 2. exd5 e6 3. d6 cxd6")
            , test "En passant (without e.p.)" <|
                \_ ->
                    Expect.equal (Ok enPassant) (Pgn.toPosition "1. e4 Nb8c6 2. e5 d5 3. exd6")
            , describe "auto test" <|
                List.map (\testCase -> test testCase.text <| \_ -> Expect.equal (Ok testCase.position) (Pgn.toPosition testCase.text)) autoTestCases
            ]
        ]
