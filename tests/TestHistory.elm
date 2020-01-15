module TestHistory exposing (..)

import Expect exposing (Expectation)
import History
import Piece exposing (PieceKind(..))
import Player exposing (Player(..))
import Ply exposing (Ply(..))
import Test exposing (..)


onePly =
    { latestPly = Just (StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }), pastMoves = [] }


twoPlies =
    { latestPly = Nothing, pastMoves = [ ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }


threePlies =
    { latestPly = Just (StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }), pastMoves = [ ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }


fourPlies =
    { latestPly = Nothing, pastMoves = [ ( StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }, StandardMove { end = { file = 2, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing } ), ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ) ] }


fivePlies =
    { latestPly = Just (StandardMove { end = { file = 1, rank = 4 }, piece = { color = WhitePlayer, kind = Bishop }, player = WhitePlayer, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing })
    , pastMoves =
        [ ( StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }
          , StandardMove { end = { file = 2, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing }
          )
        , ( StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }
          , StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing }
          )
        ]
    }


suite : Test
suite =
    describe "History"
        [ describe "getMoveNumber"
            [ test "Empty history" <|
                \_ ->
                    Expect.equal 1 (History.moveNumber History.empty)
            , test "One ply" <|
                \_ ->
                    Expect.equal 1 (History.moveNumber onePly)
            , test "Two plies" <|
                \_ ->
                    Expect.equal 2 (History.moveNumber twoPlies)
            , test "Three plies" <|
                \_ ->
                    Expect.equal 2 (History.moveNumber threePlies)
            , test "Four plies" <|
                \_ ->
                    Expect.equal 3 (History.moveNumber fourPlies)
            ]
        , describe "toList"
            [ test "Empty history" <|
                \_ ->
                    Expect.equal [] (History.toList History.empty)
            , test "One ply" <|
                \_ ->
                    Expect.equal [ StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing } ] (History.toList onePly)
            , test "Two plies" <|
                \_ ->
                    Expect.equal [ StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing } ] (History.toList twoPlies)
            , test "Three plies" <|
                \_ ->
                    Expect.equal [ StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }, StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing }, StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing } ] (History.toList threePlies)
            , test "Four plies" <|
                \_ ->
                    Expect.equal
                        [ StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }
                        , StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing }
                        , StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }
                        , StandardMove { end = { file = 2, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing }
                        ]
                        (History.toList fourPlies)
            , test "Five plies" <|
                \_ ->
                    Expect.equal
                        [ StandardMove { end = { file = 4, rank = 3 }, piece = { color = WhitePlayer, kind = Pawn }, player = WhitePlayer, promotion = Nothing, start = { file = 4, rank = 1 }, takes = Nothing }
                        , StandardMove { end = { file = 4, rank = 4 }, piece = { color = BlackPlayer, kind = Pawn }, player = BlackPlayer, promotion = Nothing, start = { file = 4, rank = 6 }, takes = Nothing }
                        , StandardMove { end = { file = 5, rank = 2 }, piece = { color = WhitePlayer, kind = Knight }, player = WhitePlayer, promotion = Nothing, start = { file = 6, rank = 0 }, takes = Nothing }
                        , StandardMove { end = { file = 2, rank = 5 }, piece = { color = BlackPlayer, kind = Knight }, player = BlackPlayer, promotion = Nothing, start = { file = 1, rank = 7 }, takes = Nothing }
                        , StandardMove { end = { file = 1, rank = 4 }, piece = { color = WhitePlayer, kind = Bishop }, player = WhitePlayer, promotion = Nothing, start = { file = 5, rank = 0 }, takes = Nothing }
                        ]
                        (History.toList fivePlies)
            ]
        ]
