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

        inBetweenSquares =
            [ 5, 6 ] |> List.map (Square rank)

        inBetweenSquaresAreClear =
            areSquaresUnoccupied position inBetweenSquares

        inBetweenSquaresNotThreatened =
            List.map (wouldMoveLeavePlayerInCheck position.playerToMove position (Square rank 4)) inBetweenSquares |> List.any identity |> not

        inCheck =
            isPlayerInCheck position.playerToMove position

        hasRookBeenCaptured =
            History.hasCaptureHappenedOnSquare (Square rank 7) position.history
    in
    not hasRookBeenCaptured && not inCheck && inBetweenSquaresAreClear && inBetweenSquaresNotThreatened && not (History.hasKingMoved position.playerToMove position.history || History.hasKingsideRookMoved position.playerToMove position.history)


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

        squaresKingWouldMoveThroughNotThreatened =
            List.map (wouldMoveLeavePlayerInCheck position.playerToMove position (Square rank 4)) squaresKingWillMoveThrough |> List.any identity |> not

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


getPossiblePawnMoves : Player -> Square -> Position -> EverySet Square
getPossiblePawnMoves player square position =
    let
        direction =
            case player of
                WhitePlayer ->
                    1

                BlackPlayer ->
                    -1

        firstMove =
            let
                candidate =
                    { square | rank = square.rank + direction }
            in
            if isSquareOccupied candidate position then
                Nothing

            else
                Just candidate

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
                            Just candidate

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
                    if data.piece.kind == Piece.Pawn && prevDistance == 2 && fileDelta == 1 && square.file == fifthRank then
                        Just (Square (square.rank + direction) data.end.file)

                    else
                        Nothing

                _ ->
                    Nothing

        captures =
            [ ( direction, -1 ), ( direction, 1 ) ]
                |> List.map (Square.offset square)
                |> List.map (omitIfNotOccupiedByPlayer position (Player.otherPlayer position.playerToMove))
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
            in
            EverySet.member sq otherPlayerPossibleMoves


generateAllPossibleNextPositions : Position -> List Position
generateAllPossibleNextPositions position =
    {- this was fun to write but is hard to read and idk if i actually need it -}
    getSquaresOccupiedByCurrentPlayer position
        |> EverySet.toList
        |> List.map (\sq -> ( sq, getPossibleMovesForCurrentPlayer position sq |> EverySet.toList ))
        |> List.map (\( start, moves ) -> List.filterMap (\end -> makeMove position start end) moves)
        |> List.concat


generateAllMoves : Player -> Position -> EverySet Square
generateAllMoves player position =
    getSquaresOccupiedByPlayer player position
        |> EverySet.toList
        |> List.map (getPossibleMoves player position)
        |> List.map EverySet.toList
        |> List.concat
        |> EverySet.fromList


getPossibleMovesForCurrentPlayer : Position -> Square -> EverySet Square
getPossibleMovesForCurrentPlayer position square =
    getPossibleMoves position.playerToMove position square


wouldMoveLeavePlayerInCheck : Player -> Position -> Square -> Square -> Bool
wouldMoveLeavePlayerInCheck player position start end =
    let
        newPosition =
            makeMove position start end
    in
    case newPosition of
        -- TODO i'm saying if the position is invalid then they're in check, because both conditions should be
        -- filtered out. this isn't strictly right but i think it will work fine. is there a more elegant way?
        Nothing ->
            True

        Just pos ->
            isPlayerInCheck player pos


getPossibleMovesForCurrentPlayerWithoutCheck : Position -> Square -> EverySet Square
getPossibleMovesForCurrentPlayerWithoutCheck position square =
    getPossibleMoves position.playerToMove position square
        |> EverySet.filter (not << wouldMoveLeavePlayerInCheck position.playerToMove position square)


getPossibleMoves : Player -> Position -> Square -> EverySet Square
getPossibleMoves player position square =
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
                    possibilities |> List.filterMap identity |> EverySet.fromList

                Piece.King ->
                    let
                        queenSideCastle =
                            if player == position.playerToMove && canQueensideCastle position then
                                [ ( 0, -2 ) ]

                            else
                                []

                        kingSideCastle =
                            if player == position.playerToMove && canKingsideCastle position then
                                [ ( 0, 2 ) ]

                            else
                                []

                        possibilities =
                            [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
                                ++ kingSideCastle
                                ++ queenSideCastle
                                |> List.map (Square.offset square)
                                |> List.map (omitIfOccupiedByPlayer position player)
                    in
                    possibilities |> List.filterMap identity |> EverySet.fromList

                Piece.Bishop ->
                    [ ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 1 ) ]
                        |> List.map (generateSquaresAlongVector player square position 8)
                        |> List.foldl EverySet.union EverySet.empty

                Piece.Queen ->
                    [ ( -1, -1 ), ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
                        |> List.map (generateSquaresAlongVector player square position 8)
                        |> List.foldl EverySet.union EverySet.empty

                Piece.Rook ->
                    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
                        |> List.map (generateSquaresAlongVector player square position 8)
                        |> List.foldl EverySet.union EverySet.empty

                Piece.Pawn ->
                    getPossiblePawnMoves player square position


makeMove : Position -> Square -> Square -> Maybe Position
makeMove position start end =
    let
        piece =
            get position start

        takenPiece =
            get position end
    in
    case piece of
        Nothing ->
            Nothing

        Just p ->
            let
                promotion =
                    if end.rank == Player.lastRank position.playerToMove && p.kind == Piece.Pawn then
                        Just { p | kind = Piece.Queen }
                        -- TODO allow promoting to other pieces

                    else
                        Nothing

                newPiece =
                    case promotion of
                        Nothing ->
                            piece

                        Just _ ->
                            promotion

                move =
                    if p.kind == Piece.King && end.file - start.file == -2 then
                        Ply.QueensideCastle position.playerToMove

                    else if p.kind == Piece.King && end.file - start.file == 2 then
                        Ply.KingsideCastle position.playerToMove

                    else
                        Ply.StandardMove
                            { player = position.playerToMove
                            , piece = p
                            , start = start
                            , end = end
                            , takes = takenPiece
                            , promotion = promotion
                            }

                newBoard =
                    case move of
                        Ply.QueensideCastle player ->
                            position.board
                                |> Array2D.set end.rank end.file (Just (Piece Piece.King player))
                                |> Array2D.set end.rank (end.file + 1) (Just (Piece Piece.Rook player))
                                |> Array2D.set start.rank start.file Nothing
                                |> Array2D.set end.rank 0 Nothing

                        Ply.KingsideCastle player ->
                            position.board
                                |> Array2D.set end.rank end.file (Just (Piece Piece.King player))
                                |> Array2D.set end.rank (end.file - 1) (Just (Piece Piece.Rook player))
                                |> Array2D.set start.rank start.file Nothing
                                |> Array2D.set end.rank 7 Nothing

                        Ply.StandardMove data ->
                            position.board
                                |> Array2D.set end.rank end.file newPiece
                                |> Array2D.set start.rank start.file Nothing
            in
            Just
                { position
                    | board = newBoard
                    , history = History.add move position.history
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
