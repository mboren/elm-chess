module Position exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)
import EverySet exposing (EverySet)
import History exposing (History)
import Piece exposing (Piece)
import Player exposing (Player(..))
import Ply exposing (Ply)
import Square exposing (File, Rank, Square)


type alias Position =
    { history : History
    , playerToMove : Player
    , board : Array2D (Maybe Piece)
    }


get : Position -> Square -> Maybe Piece
get { board } { rank, file } =
    Array2D.get rank file board
        |> Maybe.withDefault Nothing


findPieces : Piece -> Position -> List Square
findPieces piece position =
    let
        occupiedSquares =
            getSquaresOccupiedByPlayer piece.color position |> EverySet.toList

        hasPiece pos square =
            case get pos square of
                Nothing ->
                    False

                Just p ->
                    p == piece
    in
    List.filter (hasPiece position) occupiedSquares


canKingsideCastle : Position -> Bool
canKingsideCastle position =
    let
        rank =
            Player.firstRank position.playerToMove

        squaresKingWillMoveThrough =
            [ 5, 6 ] |> List.map (Square rank)

        inBetweenSquaresAreClear =
            areSquaresUnoccupied position squaresKingWillMoveThrough

        fakeKingMoves : List Ply
        fakeKingMoves =
            List.map (\sq -> Ply.StandardMove { start = Square rank 4, end = sq, piece = Piece Piece.King position.playerToMove, takes = Nothing, promotion = Nothing, player = position.playerToMove }) squaresKingWillMoveThrough

        squaresKingWouldMoveThroughNotThreatened =
            List.map (wouldMoveLeavePlayerInCheck position.playerToMove position) fakeKingMoves |> List.any identity |> not

        inCheck =
            isPlayerInCheck position.playerToMove position

        hasRookBeenCaptured =
            History.hasCaptureHappenedOnSquare (Square rank 7) position.history
    in
    not hasRookBeenCaptured && not inCheck && inBetweenSquaresAreClear && squaresKingWouldMoveThroughNotThreatened && not (History.hasKingMoved position.playerToMove position.history || History.hasKingsideRookMoved position.playerToMove position.history)


canQueensideCastle : Position -> Bool
canQueensideCastle position =
    let
        rank =
            Player.firstRank position.playerToMove

        inBetweenSquares =
            [ 1, 2, 3 ] |> List.map (Square rank)

        inBetweenSquaresAreClear =
            areSquaresUnoccupied position inBetweenSquares

        squaresKingWillMoveThrough =
            [ 2, 3 ] |> List.map (Square rank)

        fakeKingMoves : List Ply
        fakeKingMoves =
            List.map (\sq -> Ply.StandardMove { start = Square rank 4, end = sq, piece = Piece Piece.King position.playerToMove, takes = Nothing, promotion = Nothing, player = position.playerToMove }) squaresKingWillMoveThrough

        squaresKingWouldMoveThroughNotThreatened =
            List.map (wouldMoveLeavePlayerInCheck position.playerToMove position) fakeKingMoves |> List.any identity |> not

        hasRookBeenCaptured =
            History.hasCaptureHappenedOnSquare (Square rank 0) position.history
    in
    not hasRookBeenCaptured && not (isPlayerInCheck position.playerToMove position) && squaresKingWouldMoveThroughNotThreatened && inBetweenSquaresAreClear && not (History.hasKingMoved position.playerToMove position.history || History.hasQueensideRookMoved position.playerToMove position.history)


areSquaresUnoccupied : Position -> List Square -> Bool
areSquaresUnoccupied position squares =
    List.map (get position) squares |> List.filterMap identity |> List.isEmpty


getSquaresOccupiedByCurrentPlayer : Position -> EverySet Square
getSquaresOccupiedByCurrentPlayer position =
    getSquaresOccupiedByPlayer position.playerToMove position


hasPawnMovedBefore : Player -> Square -> Bool
hasPawnMovedBefore color { rank } =
    case color of
        WhitePlayer ->
            rank /= 1

        BlackPlayer ->
            rank /= 6


isSquareOccupied : Square -> Position -> Bool
isSquareOccupied { rank, file } position =
    case Array2D.get rank file position.board of
        Nothing ->
            False

        Just Nothing ->
            False

        Just _ ->
            True


isSquareOccupiedByPlayer : Position -> Player -> Square -> Bool
isSquareOccupiedByPlayer position player square =
    case get position square of
        Nothing ->
            False

        Just p ->
            p.color == player


omitIfOccupiedByPlayer : Position -> Player -> Maybe Square -> Maybe Square
omitIfOccupiedByPlayer position player maybeSquare =
    case maybeSquare of
        Nothing ->
            Nothing

        Just sq ->
            if isSquareOccupiedByPlayer position player sq then
                Nothing

            else
                maybeSquare


omitIfNotOccupiedByPlayer : Position -> Player -> Maybe Square -> Maybe Square
omitIfNotOccupiedByPlayer position player maybeSquare =
    case maybeSquare of
        Nothing ->
            Nothing

        Just sq ->
            if isSquareOccupiedByPlayer position player sq then
                maybeSquare

            else
                Nothing


getPossiblePawnMoves : Player -> Square -> Position -> EverySet Ply
getPossiblePawnMoves player square position =
    let
        direction =
            Player.direction player

        firstMove =
            let
                candidate =
                    { square | rank = square.rank + direction }

                promotion =
                    if candidate.rank == Player.lastRank player then
                        Just (Piece Piece.Queen player)

                    else
                        Nothing
            in
            if isSquareOccupied candidate position then
                Nothing

            else
                Just (Ply.StandardMove { player = player, piece = Piece Piece.Pawn player, start = square, end = candidate, takes = Nothing, promotion = promotion })

        extraMove =
            -- this checks for being blocked.
            case firstMove of
                Nothing ->
                    Nothing

                _ ->
                    if hasPawnMovedBefore player square then
                        Nothing

                    else
                        let
                            candidate =
                                { square | rank = square.rank + 2 * direction }
                        in
                        if isSquareOccupied candidate position then
                            Nothing

                        else
                            Just (Ply.StandardMove { player = player, piece = Piece Piece.Pawn player, start = square, end = candidate, takes = Nothing, promotion = Nothing })

        enpassant : Maybe Ply
        enpassant =
            let
                lp =
                    History.getLastPly position.history
            in
            case lp of
                Nothing ->
                    Nothing

                Just (Ply.StandardMove data) ->
                    let
                        prevDistance =
                            abs (data.end.rank - data.start.rank)

                        fileDelta =
                            abs (data.start.file - square.file)

                        fifthRank =
                            Player.lastRank player - (3 * direction)
                    in
                    if data.piece.kind == Piece.Pawn && prevDistance == 2 && fileDelta == 1 && square.rank == fifthRank then
                        Just (Ply.EnPassant { player = player, start = square, end = Square (square.rank + direction) data.end.file, takenPawn = data.end })

                    else
                        Nothing

                _ ->
                    Nothing

        captures =
            [ ( direction, -1 ), ( direction, 1 ) ]
                |> List.map (Square.offset square)
                |> List.filterMap (omitIfNotOccupiedByPlayer position (Player.otherPlayer position.playerToMove))
                |> List.map
                    (\s ->
                        Ply.StandardMove
                            { player = player
                            , piece = Piece Piece.Pawn player
                            , start = square
                            , end = s
                            , takes = get position s
                            , promotion =
                                if s.rank == Player.lastRank player then
                                    Just (Piece Piece.Queen player)

                                else
                                    Nothing
                            }
                    )
                |> List.map Just
    in
    [ enpassant, firstMove, extraMove ] ++ captures |> List.filterMap identity |> EverySet.fromList


omitAfterOccupied : Position -> Player -> List Square -> List Square
omitAfterOccupied position player squares =
    case squares of
        [] ->
            []

        h :: t ->
            if isSquareOccupiedByPlayer position player h then
                []

            else if isSquareOccupiedByPlayer position (Player.otherPlayer player) h then
                [ h ]

            else
                h :: omitAfterOccupied position player t


generateSquaresAlongVector : Player -> Square -> Position -> Int -> ( Int, Int ) -> EverySet Square
generateSquaresAlongVector player square position maxHops vec =
    -- assumes we can take along this vector
    -- generate all the moves
    -- recurse over the list to remove all after we hit another piece. if the piece is our own color, that square is omitted. if it's another, it is included
    let
        multiplyVector ( dr, df ) hops =
            ( dr * hops, df * hops )
    in
    List.range 1 maxHops
        |> List.map (multiplyVector vec)
        |> List.map (Square.offset square)
        |> List.filterMap identity
        |> omitAfterOccupied position player
        |> EverySet.fromList


convertEndSquareToStandardMove : Position -> Player -> Piece -> Square -> Square -> Ply
convertEndSquareToStandardMove position player piece start end =
    let
        takes =
            get position end
    in
    Ply.StandardMove { player = player, piece = piece, start = start, end = end, takes = takes, promotion = Nothing }


getKingSquare : Player -> Position -> Maybe Square
getKingSquare player position =
    let
        occupiedSquares =
            getSquaresOccupiedByPlayer player position |> EverySet.toList

        hasKing pos square =
            case get pos square of
                Nothing ->
                    False

                Just p ->
                    case p.kind of
                        Piece.King ->
                            True

                        _ ->
                            False
    in
    case List.filter (hasKing position) occupiedSquares of
        [] ->
            Nothing

        h :: [] ->
            Just h

        h :: _ ->
            Just h |> Debug.log "Found multiple kings, weird!"


isPlayerInCheck : Player -> Position -> Bool
isPlayerInCheck player position =
    let
        playerKingPosition =
            getKingSquare player position
    in
    case playerKingPosition of
        Nothing ->
            -- if they dont have a king, i guess they're not in check!
            False |> Debug.log "No king!"

        Just sq ->
            let
                otherPlayerPossibleMoves =
                    generateAllMoves (Player.otherPlayer player) position
                        |> EverySet.toList
                        |> List.filterMap Ply.toThreat
                        |> EverySet.fromList
            in
            EverySet.member sq otherPlayerPossibleMoves


isCurrentPlayerInCheckMate : Position -> Bool
isCurrentPlayerInCheckMate position =
    isPlayerInCheck position.playerToMove position
        && EverySet.empty
        == generateAllMovesForCurrentPlayerWithoutCheck position


generateAllPossibleNextPositions : Position -> List Position
generateAllPossibleNextPositions position =
    {- this was fun to write but is hard to read and idk if i actually need it -}
    getSquaresOccupiedByCurrentPlayer position
        |> EverySet.toList
        |> List.map (\sq -> ( sq, getPossibleMovesForCurrentPlayer position sq |> EverySet.toList ))
        -- TODO simplify
        |> List.map (\( start, moves ) -> List.filterMap (\ply -> makeMove position ply) moves)
        |> List.concat


generateAllMoves : Player -> Position -> EverySet Ply
generateAllMoves player position =
    getSquaresOccupiedByPlayer player position
        |> EverySet.toList
        |> List.map (getPossibleMoves False player position)
        |> List.map EverySet.toList
        |> List.concat
        |> EverySet.fromList


generateAllMovesForCurrentPlayerWithoutCheck : Position -> EverySet Ply
generateAllMovesForCurrentPlayerWithoutCheck position =
    getSquaresOccupiedByPlayer position.playerToMove position
        |> EverySet.toList
        |> List.map (getPossibleMovesForCurrentPlayerWithoutCheck position)
        |> List.map EverySet.toList
        |> List.concat
        |> EverySet.fromList


getPossibleMovesForCurrentPlayer : Position -> Square -> EverySet Ply
getPossibleMovesForCurrentPlayer position square =
    getPossibleMoves True position.playerToMove position square


wouldMoveLeavePlayerInCheck : Player -> Position -> Ply -> Bool
wouldMoveLeavePlayerInCheck player position ply =
    let
        newPosition =
            makeMove position ply
    in
    case newPosition of
        -- TODO i'm saying if the position is invalid then they're in check, because both conditions should be
        -- filtered out. this isn't strictly right but i think it will work fine. is there a more elegant way?
        Nothing ->
            True

        Just pos ->
            isPlayerInCheck player pos


getPossibleMovesForCurrentPlayerWithoutCheck : Position -> Square -> EverySet Ply
getPossibleMovesForCurrentPlayerWithoutCheck position square =
    getPossibleMoves True position.playerToMove position square
        |> EverySet.filter (not << wouldMoveLeavePlayerInCheck position.playerToMove position)


getPossibleMoves : Bool -> Player -> Position -> Square -> EverySet Ply
getPossibleMoves includeCastling player position square =
    let
        currentPiece =
            get position square
    in
    case currentPiece of
        Nothing ->
            EverySet.empty

        Just piece ->
            -- TODO i'm using both empty lists and Maybes to represent cases i dont want, and there's no
            -- reason to use both. i should pick one and make it consistent.
            case piece.kind of
                Piece.Knight ->
                    let
                        possibilities =
                            [ ( -2, -1 ), ( -2, 1 ), ( -1, -2 ), ( -1, 2 ), ( 1, -2 ), ( 1, 2 ), ( 2, -1 ), ( 2, 1 ) ]
                                |> List.map (Square.offset square)
                                |> List.map (omitIfOccupiedByPlayer position player)
                    in
                    possibilities |> List.filterMap identity |> EverySet.fromList |> EverySet.map (convertEndSquareToStandardMove position player piece square)

                Piece.King ->
                    let
                        queenSideCastle =
                            if includeCastling && player == position.playerToMove && canQueensideCastle position then
                                [ Ply.QueensideCastle player ]

                            else
                                []

                        kingSideCastle =
                            if includeCastling && player == position.playerToMove && canKingsideCastle position then
                                [ Ply.KingsideCastle player ]

                            else
                                []

                        possibilities =
                            [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
                                |> List.map (Square.offset square)
                                |> List.map (omitIfOccupiedByPlayer position player)
                    in
                    possibilities
                        |> List.filterMap identity
                        |> List.map (convertEndSquareToStandardMove position player piece square)
                        |> (++) kingSideCastle
                        |> (++) queenSideCastle
                        |> EverySet.fromList

                Piece.Bishop ->
                    [ ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 1 ) ]
                        |> List.map (generateSquaresAlongVector player square position 8)
                        |> List.foldl EverySet.union EverySet.empty
                        |> EverySet.map (convertEndSquareToStandardMove position player piece square)

                Piece.Queen ->
                    [ ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
                        |> List.map (generateSquaresAlongVector player square position 8)
                        |> List.foldl EverySet.union EverySet.empty
                        |> EverySet.map (convertEndSquareToStandardMove position player piece square)

                Piece.Rook ->
                    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
                        |> List.map (generateSquaresAlongVector player square position 8)
                        |> List.foldl EverySet.union EverySet.empty
                        |> EverySet.map (convertEndSquareToStandardMove position player piece square)

                Piece.Pawn ->
                    getPossiblePawnMoves player square position


makeMove : Position -> Ply -> Maybe Position
makeMove position ply =
    let
        newBoard =
            case ply of
                Ply.QueensideCastle player ->
                    let
                        rank =
                            Player.firstRank player

                        kingFile =
                            2

                        rookFile =
                            3
                    in
                    position.board
                        |> Array2D.set rank kingFile (Just (Piece Piece.King player))
                        |> Array2D.set rank rookFile (Just (Piece Piece.Rook player))
                        |> Array2D.set rank 4 Nothing
                        |> Array2D.set rank 0 Nothing

                Ply.KingsideCastle player ->
                    let
                        rank =
                            Player.firstRank player

                        kingFile =
                            6

                        rookFile =
                            5
                    in
                    position.board
                        |> Array2D.set rank kingFile (Just (Piece Piece.King player))
                        |> Array2D.set rank rookFile (Just (Piece Piece.Rook player))
                        |> Array2D.set rank 4 Nothing
                        |> Array2D.set rank 7 Nothing

                Ply.StandardMove data ->
                    let
                        newPiece =
                            case data.promotion of
                                Nothing ->
                                    Just data.piece

                                Just pp ->
                                    Just pp
                    in
                    position.board
                        |> Array2D.set data.end.rank data.end.file newPiece
                        |> Array2D.set data.start.rank data.start.file Nothing

                Ply.EnPassant data ->
                    position.board
                        |> Array2D.set data.start.rank data.start.file Nothing
                        |> Array2D.set data.end.rank data.end.file (Just (Piece Piece.Pawn data.player))
                        |> Array2D.set data.takenPawn.rank data.takenPawn.file Nothing
    in
    Just
        { position
            | board = newBoard
            , history = History.add ply position.history
            , playerToMove = Player.otherPlayer position.playerToMove
        }


getSquaresOccupiedByPlayer : Player -> Position -> EverySet Square
getSquaresOccupiedByPlayer color position =
    let
        getSquareIfColorMatches rank file maybePiece =
            case maybePiece of
                Nothing ->
                    Nothing

                Just piece ->
                    if piece.color == color then
                        Just (Square rank file)

                    else
                        Nothing
    in
    Array2D.indexedMap getSquareIfColorMatches position.board
        |> getRows
        |> List.concat
        -- get rid of nothings
        |> List.filterMap identity
        |> EverySet.fromList


getRows : Array2D a -> List (List a)
getRows board =
    {- TODO rename this -}
    let
        maybeToBool r =
            --TODO i'm sure there's a smoother way to do this
            case r of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    List.range 0 (Array2D.rows board)
        |> List.map (\i -> Array2D.getRow i board)
        |> List.filter maybeToBool
        |> List.map (Maybe.withDefault (Array.fromList []))
        |> List.map Array.toList
        |> List.reverse


initial =
    { history = History.empty
    , playerToMove = WhitePlayer

    {- i'm still playing with both the Piece type definition and how i'm storing the board
       so until that settles down
       i'm just gonna convert from string here, because it's easier to change the function
       definition than all the dang pieces on the board
    -}
    , board =
        [ [ "R", "N", "B", "Q", "K", "B", "N", "R" ]
        , [ "P", "P", "P", "P", "P", "P", "P", "P" ]
        , [ " ", " ", " ", " ", " ", " ", " ", " " ]
        , [ " ", " ", " ", " ", " ", " ", " ", " " ]
        , [ " ", " ", " ", " ", " ", " ", " ", " " ]
        , [ " ", " ", " ", " ", " ", " ", " ", " " ]
        , [ "p", "p", "p", "p", "p", "p", "p", "p" ]
        , [ "r", "n", "b", "q", "k", "b", "n", "r" ]
        ]
            |> List.map (List.map Piece.fromString)
            |> Array2D.fromList
    }


fromPgn : String -> Result String Position
fromPgn text =
    if String.isEmpty text then
        Ok initial

    else
        Err "Not handled yet"
