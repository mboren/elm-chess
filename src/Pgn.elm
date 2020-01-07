module Pgn exposing (..)

import Parser exposing (..)
import Piece exposing (Piece)
import Ply exposing (Ply)
import Position exposing (Position)
import Square exposing (Square)


type PgnPly
    = KingsideCastle
    | QueensideCastle
    | PawnAdvance Square (Maybe Piece)
    | PawnCapture { startFile : Square.File, end : Square, promotion : Maybe Piece.PieceKind }
    | Capture { pieceKind : Piece.PieceKind, startRank : Maybe Square.Rank, startFile : Maybe Square.File, end : Square }
    | Standard { pieceKind : Piece.PieceKind, startRank : Maybe Square.Rank, startFile : Maybe Square.File, end : Square }


pawnAdvance : Parser PgnPly
pawnAdvance =
    square |> andThen (\s -> succeed (PawnAdvance s Nothing))


pawnCapture : Parser PgnPly
pawnCapture =
    succeed (\f e -> PawnCapture { startFile = f, end = e, promotion = Nothing })
        |= file
        |. symbol "x"
        |= square


pieceKind : Parser Piece.PieceKind
pieceKind =
    chompIf isValidPieceKind
        |> getChompedString
        |> andThen stringToPieceKind


stringToPieceKind : String -> Parser Piece.PieceKind
stringToPieceKind s =
    case s of
        "P" ->
            succeed Piece.Pawn

        "R" ->
            succeed Piece.Rook

        "N" ->
            succeed Piece.Knight

        "B" ->
            succeed Piece.Bishop

        "K" ->
            succeed Piece.King

        "Q" ->
            succeed Piece.Queen

        "" ->
            problem "empty string is not a valid piece kind"

        _ ->
            problem ("invalid piece kind: " ++ s)


moveNumber : Parser ()
moveNumber =
    succeed ()
        |. chompWhile (\c -> Char.isDigit c && c /= '0')
        |. symbol "."


square : Parser Square
square =
    succeed (\f r -> Square r f)
        |= file
        |= rank


rank : Parser Square.Rank
rank =
    int |> andThen intToRank


file : Parser Square.File
file =
    chompIf isValidFileChar
        |> getChompedString
        |> andThen stringToFile


isValidFileChar : Char -> Bool
isValidFileChar c =
    Char.toCode 'a' <= Char.toCode c && Char.toCode c <= Char.toCode 'h'


isValidPieceKind : Char -> Bool
isValidPieceKind c =
    List.member c [ 'R', 'N', 'B', 'K', 'Q' ]


stringToFile : String -> Parser Square.File
stringToFile s =
    case String.uncons s of
        Just ( c, "" ) ->
            succeed (unsafeCharToFile c)

        Just ( c, t ) ->
            problem ("Multiple chars found for file: " ++ s)

        Nothing ->
            problem "empty string is not a valid file"


unsafeCharToFile : Char -> Square.File
unsafeCharToFile c =
    Char.toCode c - Char.toCode 'a'


intToRank : Int -> Parser Square.Rank
intToRank i =
    if i >= 1 && i <= 8 then
        succeed (i - 1)

    else
        problem ("Invalid rank: " ++ String.fromInt i ++ ", must be in [1,8]")
