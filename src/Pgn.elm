module Pgn exposing (..)

import Parser exposing (..)
import Piece exposing (Piece)
import Square exposing (Square)


type PgnPly
    = KingsideCastle
    | QueensideCastle
    | PawnAdvance Square (Maybe Piece.PieceKind)
    | PawnCapture { startFile : Square.File, end : Square, promotion : Maybe Piece.PieceKind }
    | Capture { pieceKind : Piece.PieceKind, startRank : Maybe Square.Rank, startFile : Maybe Square.File, end : Square }
    | Standard { pieceKind : Piece.PieceKind, startRank : Maybe Square.Rank, startFile : Maybe Square.File, end : Square }


moves : Parser (List PgnPly)
moves =
    loop [] movesHelp


movesHelp : List PgnPly -> Parser (Step (List PgnPly) (List PgnPly))
movesHelp plies =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (Loop plies)
                |. moveNumber
            , succeed (\p -> Loop (p :: plies))
                |= ply
            , succeed ()
                |> map (\_ -> Done (List.reverse plies))
            ]
        |. annotationChars

annotationChars : Parser ()
annotationChars =
    chompWhile (\c->c == '+' || c == '!' || c == '?' || c == '#')

ply : Parser PgnPly
ply =
    oneOf
        [ castle
        , pawnPly
        , regularPly
        ]


stringToCastle s =
    case s of
        "O-O" ->
            succeed KingsideCastle

        "O-O-O" ->
            succeed QueensideCastle

        _ ->
            problem ("I thought I was looking at castling, but found something weird: " ++ s)


castle : Parser PgnPly
castle =
    chompWhile (\c -> c == 'O' || c == '-')
        |> getChompedString
        |> andThen stringToCastle


type PlyPart
    = RankPart Square.Rank
    | FilePart Square.File
    | CapturePart


plyPart : Parser (Maybe PlyPart)
plyPart =
    oneOf
        [ succeed (Just << RankPart)
            |= rank
        , succeed (Just << FilePart)
            |= file
        , succeed (Just CapturePart)
            |. symbol "x"
        , succeed Nothing
        ]


constructStandardPlyFromParts : Piece.PieceKind -> Maybe PlyPart -> Maybe PlyPart -> Maybe PlyPart -> Maybe PlyPart -> Maybe PlyPart -> Maybe PgnPly
constructStandardPlyFromParts piece part1 part2 part3 part4 part5 =
    let
        parts =
            [ part1, part2, part3, part4, part5 ]
    in
    case parts of
        [ Just (FilePart f), Just (RankPart r), Nothing, Nothing, Nothing ] ->
            Just (Standard { pieceKind = piece, startRank = Nothing, startFile = Nothing, end = Square r f })

        [ Just CapturePart, Just (FilePart f), Just (RankPart r), Nothing, Nothing ] ->
            Just (Capture { pieceKind = piece, startRank = Nothing, startFile = Nothing, end = Square r f })

        [ Just (FilePart sf), Just (FilePart f), Just (RankPart r), Nothing, Nothing ] ->
            Just (Standard { pieceKind = piece, startRank = Nothing, startFile = Just sf, end = Square r f })

        [ Just (FilePart sf), Just CapturePart, Just (FilePart f), Just (RankPart r), Nothing ] ->
            Just (Capture { pieceKind = piece, startRank = Nothing, startFile = Just sf, end = Square r f })

        [ Just (RankPart sr), Just (FilePart f), Just (RankPart r), Nothing, Nothing ] ->
            Just (Standard { pieceKind = piece, startRank = Just sr, startFile = Nothing, end = Square r f })

        [ Just (RankPart sr), Just CapturePart, Just (FilePart f), Just (RankPart r), Nothing ] ->
            Just (Capture { pieceKind = piece, startRank = Just sr, startFile = Nothing, end = Square r f })

        [ Just (FilePart sf), Just (RankPart sr), Just (FilePart f), Just (RankPart r), Nothing ] ->
            Just (Standard { pieceKind = piece, startRank = Just sr, startFile = Just sf, end = Square r f })

        [ Just (FilePart sf), Just (RankPart sr), Just CapturePart, Just (FilePart f), Just (RankPart r) ] ->
            Just (Capture { pieceKind = piece, startRank = Just sr, startFile = Just sf, end = Square r f })

        _ ->
            Nothing


regularPly : Parser PgnPly
regularPly =
    succeed constructStandardPlyFromParts
        |= pieceKind
        |= plyPart
        |= plyPart
        |= plyPart
        |= plyPart
        |= plyPart
        |> andThen
            (\maybe ->
                case maybe of
                    Nothing ->
                        problem "found an invalid ply"

                    Just pgnPly ->
                        succeed pgnPly
            )


type PawnTargetData
    = AdvanceData Square.Rank
    | CaptureData Square


pawnTarget : Parser PawnTargetData
pawnTarget =
    oneOf
        [ succeed AdvanceData
            |= rank
        , succeed CaptureData
            |. symbol "x"
            |= square
        ]


constructPawnPly : Square.File -> PawnTargetData -> Maybe Piece.PieceKind -> PgnPly
constructPawnPly startFile target parsedPromotion =
    case target of
        AdvanceData r ->
            PawnAdvance (Square r startFile) parsedPromotion

        CaptureData s ->
            PawnCapture { startFile = startFile, end = s, promotion = parsedPromotion }


pawnPly : Parser PgnPly
pawnPly =
    succeed constructPawnPly
        |= file
        |= pawnTarget
        |= promotion


promotion : Parser (Maybe Piece.PieceKind)
promotion =
    oneOf
        [ succeed Just
            |. symbol "="
            |= pieceKind
        , succeed Nothing
        ]


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
        |. chompIf Char.isDigit
        |. chompWhile Char.isDigit
        |. symbol "."


square : Parser Square
square =
    succeed (\f r -> Square r f)
        |= file
        |= rank


rank : Parser Square.Rank
rank =
    chompIf isValidRankChar
        |> getChompedString
        |> andThen stringToRank


file : Parser Square.File
file =
    chompIf isValidFileChar
        |> getChompedString
        |> andThen stringToFile


isValidRankChar : Char -> Bool
isValidRankChar c =
    Char.toCode '1' <= Char.toCode c && Char.toCode c <= Char.toCode '8'


isValidFileChar : Char -> Bool
isValidFileChar c =
    Char.toCode 'a' <= Char.toCode c && Char.toCode c <= Char.toCode 'h'


isValidPieceKind : Char -> Bool
isValidPieceKind c =
    List.member c [ 'R', 'N', 'B', 'K', 'Q' ]


stringToRank : String -> Parser Square.Rank
stringToRank s =
    case String.uncons s of
        Just ( c, "" ) ->
            succeed (Char.toCode c - Char.toCode '1')

        Just ( c, t ) ->
            problem ("Multiple chars found for file: " ++ s)

        Nothing ->
            problem "empty string is not a valid file"


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


parsingErrorToString : DeadEnd -> String
parsingErrorToString deadEnd =
    let
        location = "(row " ++ String.fromInt deadEnd.row ++ ", col " ++ String.fromInt deadEnd.col ++ ")"
        mainText =
            case deadEnd.problem of
                Expecting expectedString -> "was expecting to find \"" ++ expectedString ++ "\""
                ExpectingInt -> "expected an integer"
                ExpectingSymbol symbol -> "expected the symbol " ++ symbol
                ExpectingEnd -> "expected the string to end"
                UnexpectedChar -> "found a character I was not expecting"
                Problem problemText -> "ran into the error: \"" ++ problemText ++ "\""
                BadRepeat -> "ran into a BadRepeat (idk what this means)"
                _ -> "ran into an unexpected problem"
    in
    "I " ++ mainText ++ " at " ++ location

parsingErrorsToString : List DeadEnd -> String
parsingErrorsToString errors =
    List.map (parsingErrorToString) errors
    |> String.join ", "


