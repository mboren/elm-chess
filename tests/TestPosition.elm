module TestPosition exposing (..)

import Array
import EverySet
import Expect exposing (Expectation)
import Pgn
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


stalematePosition =
    let
        parsedPosition =
            Pgn.toPosition """1. c4 Nf6 2. d4 e6 3. Nc3 Bb4 4. e3 c5 5. Nge2 d5 6. a3 Bxc3
7. Nxc3 cxd4 8. exd4 dxc4 9. Bxc4 Nc6 10. Be3 O-O 11. O-O b6
12. Qd3 Bb7 13. Rad1 h6 14. f3 Ne7 15. Bf2 Nfd5 16. Ba2 Nf4
17. Qd2 Nfg6 18. Bb1 Qd7 19. h4 Rfd8 20. h5 Nf8 21. Bh4 f6
22. Ne4 Nd5 23. g4 Rac8 24. Bg3 Ba6 25. Rfe1 Rc6 26. Rc1 Ne7
27. Rxc6 Qxc6 28. Ba2 Qd7 29. Nd6 Bb7 30. Nxb7 Qxb7 31. Qe3
Kh8 32. Rc1 Nd5 33. Qe4 Qd7 34. Bb1 Qb5 35. b4 Qd7 36. Qd3 Qe7
37. Kf2 f5 38. gxf5 exf5 39. Re1 Qf6 40. Be5 Qh4 41. Bg3 Qf6
42. Rh1 Nh7 43. Be5 Qg5 44. Qxf5 Qd2 45. Kg3 Nhf6 46. Rg1 Re8
47. Be4 Ne7 48. Qh3 Rc8 49. Kh4 Rc1 50. Qg3 Rxg1 51. Qxg1 Kg8
52. Qg3 Kf7 53. Bg6 Ke6 54. Qh3 Kd5 55. Be4 Nxe4 56. fxe4
Kxe4 57. Qg4 Kd3 58. Qf3 Qe3 59. Kg4 Qxf3 60. Kxf3 g6
61. Bd6 Nf5 62. Kf4 Nh4 63. Kg4 gxh5 64. Kxh4 Kxd4 65. Bb8 a5
66. Bd6 Kc4 67. Kxh5 a4 68. Kxh6 Kb3 69. b5 Kc4 70. Kg5 Kxb5
71. Kf5 Ka6 72. Ke6 Ka7 73. Kd7 Kb7 74. Be7 Ka7 75. Kc7 Ka8
76. Bd6 Ka7 77. Kc8 Ka6 78. Kb8 b5 79. Bb4 Kb6 80. Kc8 Kc6
81. Kd8 Kd5 82. Ke7 Ke5 83. Kf7 Kd5 84. Kf6 Kd4 85. Ke6 Ke4
86. Bf8 Kd4 87. Kd6 Ke4 88. Bg7 Kf4 89. Ke6 Kf3 90. Ke5 Kg4
91. Bf6 Kh5 92. Kf5 Kh6 93. Bd4 Kh7 94. Kf6 Kh6 95. Be3 Kh5
96. Kf5 Kh4 97. Bd2 Kg3 98. Bg5 Kf3 99. Bf4 Kg2 100. Bd6 Kf3
101. Bh2 Kg2 102. Bc7 Kf3 103. Bd6 Ke3 104. Ke5 Kf3 105. Kd5
Kg4 106. Kc5 Kf5 107. Kxb5 Ke6 108. Kc6 Kf6 109. Kd7 Kf7
110. Be7 Kg8 111. Ke6 Kg7 112. Bc5 Kg8 113. Kf6 Kh7 114. Kf7
Kh8 115. Bd4 Kh7 116. Bb2 Kh6 117. Kg8 Kg6 118. Bg7 Kf5
119. Kf7 Kg5 120. Bb2 Kh6 121. Bc1 Kh7 122. Bd2 Kh8 123. Bc3
Kh7 124. Bg7"""
    in
    case parsedPosition of
        Ok pos ->
            pos

        Err err ->
            Position.initial


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
                        position =
                            { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Nothing, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Nothing, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = White, kind = Knight }, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Nothing, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (StandardMove { end = { file = 1, rank = 4 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 2, rank = 2 }, takes = Nothing }), pastMoves = [ ( StandardMove { end = { file = 3, rank = 3 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 5, rank = 2 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 2, rank = 5 }, takes = Nothing } ), ( StandardMove { end = { file = 2, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 1, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 2, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ) ] }, playerToMove = Black }
                    in
                    Expect.equal "1. Nf3 Nf6 2. Nc3 Nc6 3. Nd4 Ne5 4. Ncb5" (Position.toPgn position)
            , test "single pawn move" <|
                \_ ->
                    let
                        position =
                            { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }), pastMoves = [] }, playerToMove = Black }
                    in
                    Expect.equal "1. e4" (Position.toPgn position)
            , test "two plies" <|
                \_ ->
                    let
                        position =
                            { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Just { color = White, kind = Knight }, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Nothing, pastMoves = [ ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = White }
                    in
                    Expect.equal "1. e4 e5" (Position.toPgn position)
            , test "three plies" <|
                \_ ->
                    let
                        position =
                            { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Just { color = White, kind = King }, Just { color = White, kind = Bishop }, Nothing, Just { color = White, kind = Rook } ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (StandardMove { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }), pastMoves = [ ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = Black }
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
                        positionBeforeCastling =
                            { board = { columns = 8, data = Array.fromList [ Array.fromList [ Just { color = White, kind = Rook }, Just { color = White, kind = Knight }, Just { color = White, kind = Bishop }, Just { color = White, kind = Queen }, Nothing, Just { color = White, kind = Rook }, Just { color = White, kind = King }, Nothing ], Array.fromList [ Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Nothing, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn }, Just { color = White, kind = Pawn } ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = White, kind = Knight }, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = Black, kind = Bishop }, Nothing, Nothing, Just { color = White, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Just { color = White, kind = Bishop }, Nothing, Nothing, Just { color = Black, kind = Pawn }, Nothing, Nothing, Nothing ], Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Just { color = Black, kind = Knight }, Nothing, Nothing ], Array.fromList [ Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Nothing, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn }, Just { color = Black, kind = Pawn } ], Array.fromList [ Just { color = Black, kind = Rook }, Just { color = Black, kind = Knight }, Just { color = Black, kind = Bishop }, Just { color = Black, kind = Queen }, Just { color = Black, kind = King }, Nothing, Nothing, Just { color = Black, kind = Rook } ] ] }, history = { latestPly = Just (KingsideCastle White), pastMoves = [ ( StandardMove { end = { file = 5, rank = 2 }, piece = { color = White, kind = Knight }, player = White, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 1, rank = 3 }, piece = { color = Black, kind = Bishop }, player = Black, promotion = Nothing, start = { file = 5, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 1, rank = 4 }, piece = { color = White, kind = Bishop }, player = White, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 5, rank = 5 }, piece = { color = Black, kind = Knight }, player = Black, promotion = Nothing, start = { file = 6, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = White, kind = Pawn }, player = White, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = Black, kind = Pawn }, player = Black, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }, playerToMove = Black }
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
        , describe "isStalemate"
            [ test "initial position should not be stalemate" <|
                \_ ->
                    Expect.equal False (Position.isStalemate Position.initial)
            , test "Korchnoi vs. Karpov, 1978 should be stalemate" <|
                \_ ->
                    Expect.equal True (Position.isStalemate stalematePosition)
            ]
        ]
