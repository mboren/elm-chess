module Position exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)
import EverySet exposing (EverySet)
import History exposing (History)
import Parser
import Pgn
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


isPlyValid : Ply -> Position -> Result String ()
isPlyValid ply position =
    let
        startSquare =
            Ply.getStart ply

        possiblePlies =
            getPossibleMovesForCurrentPlayerWithoutCheck position startSquare
    in
    if EverySet.member ply possiblePlies then
        Ok ()
    else if EverySet.member ply (getPossibleMovesForCurrentPlayerIgnoringCheck position startSquare) then
        Err ("Moving from " ++ Square.toString (Ply.getStart ply) ++ " would result in check!")
    else
        Err "Illegal ply"


doesCurrentPlayerHavePawnOnSquare : Square -> Position -> Bool
doesCurrentPlayerHavePawnOnSquare square position =
    case get position square of
        Nothing ->
            False

        Just p ->
            p.kind == Piece.Pawn && p.color == position.playerToMove


findPawnThatCanMoveToSquare : Square -> Position -> Maybe Square
findPawnThatCanMoveToSquare square position =
    let
        player =
            position.playerToMove

        direction =
            Player.direction player

        oneBack =
            Square.offset square ( -1 * direction, 0 )

        twoBack =
            if square.rank - (2 * direction) == direction + Player.firstRank player then
                Square.offset square ( -2 * direction, 0 )

            else
                Nothing

        -- note that i'm not checking if the squares are occupied bc i think that'll be taken care of in a later stage
    in
    case oneBack of
        Nothing ->
            Nothing

        Just oneBackSquare ->
            if doesCurrentPlayerHavePawnOnSquare oneBackSquare position then
                Just oneBackSquare

            else
                case twoBack of
                    Nothing ->
                        Nothing

                    Just twoBackSquare ->
                        if doesCurrentPlayerHavePawnOnSquare twoBackSquare position then
                            Just twoBackSquare

                        else
                            Nothing


canKingsideCastle : Position -> Bool
canKingsideCastle position =
    let
        rank =
            Player.firstRank position.playerToMove

        squaresKingWillMoveThrough =
            [ 5, 6 ] |> List.map (Square rank)

        inBetweenSquaresAreClear =
            areSquaresUnoccupied position squaresKingWillMoveThrough

        inCheck =
            isPlayerInCheck position.playerToMove position

        hasRookBeenCaptured =
            History.hasCaptureHappenedOnSquare (Square rank 7) position.history
    in
    if not hasRookBeenCaptured && not inCheck && inBetweenSquaresAreClear && not (History.hasKingMoved position.playerToMove position.history || History.hasKingsideRookMoved position.playerToMove position.history) then
        let
            fakeKingMoves : List Ply
            fakeKingMoves =
                List.map (\sq -> Ply.StandardMove { start = Square rank 4, end = sq, piece = Piece Piece.King position.playerToMove, takes = Nothing, promotion = Nothing, player = position.playerToMove }) squaresKingWillMoveThrough

            squaresKingWouldMoveThroughNotThreatened =
                List.map (wouldMoveLeavePlayerInCheck position.playerToMove position) fakeKingMoves |> List.any identity |> not
        in
        squaresKingWouldMoveThroughNotThreatened

    else
        False


canQueensideCastle : Position -> Bool
canQueensideCastle position =
    let
        rank =
            Player.firstRank position.playerToMove

        inBetweenSquares =
            [ 1, 2, 3 ] |> List.map (Square rank)

        inBetweenSquaresAreClear =
            areSquaresUnoccupied position inBetweenSquares

        hasRookBeenCaptured =
            History.hasCaptureHappenedOnSquare (Square rank 0) position.history
    in
    if not (History.hasKingMoved position.playerToMove position.history || History.hasQueensideRookMoved position.playerToMove position.history) && not hasRookBeenCaptured && not (isPlayerInCheck position.playerToMove position) && inBetweenSquaresAreClear then
        let
            squaresKingWillMoveThrough =
                [ 2, 3 ] |> List.map (Square rank)

            fakeKingMoves : List Ply
            fakeKingMoves =
                List.map (\sq -> Ply.StandardMove { start = Square rank 4, end = sq, piece = Piece Piece.King position.playerToMove, takes = Nothing, promotion = Nothing, player = position.playerToMove }) squaresKingWillMoveThrough

            squaresKingWouldMoveThroughNotThreatened =
                List.map (wouldMoveLeavePlayerInCheck position.playerToMove position) fakeKingMoves |> List.any identity |> not
        in
        squaresKingWouldMoveThroughNotThreatened

    else
        False


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

getPossibleMovesForCurrentPlayerIgnoringCheck : Position -> Square -> EverySet Ply
getPossibleMovesForCurrentPlayerIgnoringCheck position square =
    getPossibleMoves True position.playerToMove position square

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


canPieceMoveBetweenSquares : Position -> Square -> Square -> Bool
canPieceMoveBetweenSquares position start end =
    getPossibleMovesForCurrentPlayerWithoutCheck position start
        |> EverySet.toList
        |> List.filterMap Ply.getEnd
        |> List.member end


applyPgnPly : Pgn.PgnPly -> Result String Position -> Result String Position
applyPgnPly p position =
    case position of
        Err e ->
            Err e

        Ok pos ->
            let
                player =
                    pos.playerToMove

                plyResult =
                    case p of
                        Pgn.KingsideCastle ->
                            Ok (Ply.KingsideCastle pos.playerToMove)

                        Pgn.QueensideCastle ->
                            Ok (Ply.QueensideCastle pos.playerToMove)

                        Pgn.PawnAdvance endSquare maybePromotion ->
                            case findPawnThatCanMoveToSquare endSquare pos of
                                Nothing ->
                                    Err ("can't find pawn that can move to " ++ Square.toString endSquare)

                                Just square ->
                                    Ok (Ply.StandardMove { player = pos.playerToMove, piece = Piece Piece.Pawn pos.playerToMove, start = square, end = endSquare, takes = Nothing, promotion = Maybe.map (\kind -> Piece kind pos.playerToMove) maybePromotion })

                        Pgn.PawnCapture data ->
                            let
                                direction =
                                    Player.direction pos.playerToMove

                                pawnSquare =
                                    Square (data.end.rank - direction) data.startFile

                                capturedPiece =
                                    get pos data.end

                                enPassantCaptureSquare =
                                    Square pawnSquare.rank data.end.file

                                enPassantCapture =
                                    get pos enPassantCaptureSquare
                            in
                            case capturedPiece of
                                Nothing ->
                                    case enPassantCapture of
                                        Nothing ->
                                            Err ("No piece at " ++ Square.toString data.end ++ ", or " ++ Square.toString enPassantCaptureSquare ++ " to capture")

                                        Just cp ->
                                            if cp.kind == Piece.Pawn then
                                                Ok
                                                    (Ply.EnPassant
                                                        { player = player
                                                        , start = pawnSquare
                                                        , end = data.end
                                                        , takenPawn = Square pawnSquare.rank data.end.file
                                                        }
                                                    )

                                            else
                                                Err ("No piece at " ++ Square.toString data.end ++ " to capture")

                                Just cp ->
                                    Ok (Ply.StandardMove { player = pos.playerToMove, piece = Piece Piece.Pawn pos.playerToMove, start = pawnSquare, end = data.end, takes = capturedPiece, promotion = Maybe.map (\kind -> Piece kind pos.playerToMove) data.promotion })

                        Pgn.Standard data ->
                            let
                                possibleStartSquares =
                                    findPieces (Piece data.pieceKind pos.playerToMove) pos |> List.filter (\s -> canPieceMoveBetweenSquares pos s data.end)

                                maybeTakenPiece =
                                    get pos data.end
                            in
                            case maybeTakenPiece of
                                Just takenPiece ->
                                    Err ("Non-capturing move ends up on a square (" ++ Square.toString data.end ++ ") with a piece on it")

                                -- if there was actually a piece there to take, something is wrong
                                Nothing ->
                                    case ( data.startRank, data.startFile ) of
                                        ( Just sr, Just sf ) ->
                                            if List.member (Square sr sf) possibleStartSquares then
                                                Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = Square sr sf, end = data.end, takes = Nothing, promotion = Nothing })

                                            else
                                                Err ("(r,f) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                        ( Just sr, Nothing ) ->
                                            let
                                                candidates =
                                                    List.filter (.rank >> (==) sr) possibleStartSquares
                                            in
                                            case candidates of
                                                [] ->
                                                    Err ("(r,_) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                                [ h ] ->
                                                    Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = Square sr h.file, end = data.end, takes = Nothing, promotion = Nothing })

                                                h :: t ->
                                                    Err ("(r,_) There are multiple " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                        ( Nothing, Just sf ) ->
                                            let
                                                candidates =
                                                    List.filter (.file >> (==) sf) possibleStartSquares
                                            in
                                            case candidates of
                                                [] ->
                                                    Err ("(_,f) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                                [ h ] ->
                                                    Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = Square h.rank sf, end = data.end, takes = Nothing, promotion = Nothing })

                                                h :: t ->
                                                    Err ("(_,f) There are multiple " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                        ( Nothing, Nothing ) ->
                                            case possibleStartSquares of
                                                [] ->
                                                    Err ("(_,_) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                                [ h ] ->
                                                    Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = h, end = data.end, takes = maybeTakenPiece, promotion = Nothing })

                                                h :: t ->
                                                    Err ("(_,_) There are multiple " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                        Pgn.Capture data ->
                            let
                                possibleStartSquares =
                                    findPieces (Piece data.pieceKind pos.playerToMove) pos |> List.filter (\s -> canPieceMoveBetweenSquares pos s data.end)

                                maybeTakenPiece =
                                    get pos data.end
                            in
                            case maybeTakenPiece of
                                Nothing ->
                                    Err ("No piece to take at " ++ Square.toString data.end)

                                -- if there wasnt actually a piece there to take, something is wrong
                                Just takenPiece ->
                                    case ( data.startRank, data.startFile ) of
                                        ( Just sr, Just sf ) ->
                                            if List.member (Square sr sf) possibleStartSquares then
                                                Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = Square sr sf, end = data.end, takes = maybeTakenPiece, promotion = Nothing })

                                            else
                                                Err ("(r,f) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                        ( Just sr, Nothing ) ->
                                            let
                                                candidates =
                                                    List.filter (.rank >> (==) sr) possibleStartSquares
                                            in
                                            case candidates of
                                                [] ->
                                                    Err ("(r,_) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                                [ h ] ->
                                                    Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = Square sr h.file, end = data.end, takes = maybeTakenPiece, promotion = Nothing })

                                                h :: t ->
                                                    Err ("(r,_) There are multiple " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                        ( Nothing, Just sf ) ->
                                            let
                                                candidates =
                                                    List.filter (.file >> (==) sf) possibleStartSquares
                                            in
                                            case candidates of
                                                [] ->
                                                    Err ("(_,f) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                                [ h ] ->
                                                    Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = Square h.rank sf, end = data.end, takes = maybeTakenPiece, promotion = Nothing })

                                                h :: t ->
                                                    Err ("(_,f) There are multiple " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                        ( Nothing, Nothing ) ->
                                            case possibleStartSquares of
                                                [] ->
                                                    Err ("(_,_) There is no " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)

                                                [ h ] ->
                                                    Ok (Ply.StandardMove { player = player, piece = Piece data.pieceKind player, start = h, end = data.end, takes = maybeTakenPiece, promotion = Nothing })

                                                h :: t ->
                                                    Err ("(_,_) There are multiple " ++ Piece.pieceKindToString data.pieceKind ++ " that can move to " ++ Square.toString data.end)
            in
            case plyResult of
                Err e ->
                    Err e

                Ok ply ->
                    case isPlyValid ply pos of
                        Ok _ ->
                            makeMove pos ply |> Result.fromMaybe "Failed to make move!"
                        Err err ->
                            let
                                _ =
                                    Debug.log "Invalid ply: " ply
                            in
                            Err err

fromPgn : String -> Result String Position
fromPgn text =
    if String.isEmpty text then
        Ok initial

    else
        let
            parsedPlies =
                Parser.run Pgn.moves text
        in
        case parsedPlies of
            Ok plies ->
                List.foldl applyPgnPly (Ok initial) plies

            Err err ->
                let
                    _ =
                        Debug.log "Parsing error" err
                in
                Err "Parsing error"


plyToString : Position -> Ply -> String
plyToString position ply =
    case ply of
        Ply.StandardMove data ->
            let
                pieceString =
                    case data.piece.kind of
                        Piece.Pawn ->
                            case data.takes of
                                Nothing ->
                                    ""

                                Just _ ->
                                    Square.fileToString data.start.file

                        _ ->
                            Piece.pieceKindToString data.piece.kind |> String.toUpper

                matchingPieces =
                    findPieces data.piece position

                casesToDisambiguate =
                    List.filter (\s -> canPieceMoveBetweenSquares position s data.end) matchingPieces

                context =
                    case casesToDisambiguate of
                        [] ->
                            "" |> Debug.log "shouldnt happen"

                        [ _ ] ->
                            ""

                        h :: t ->
                            if 1 == List.length (List.filter (.file >> (==) data.start.file) casesToDisambiguate) then
                                if data.piece.kind /= Piece.Pawn then
                                    Square.fileToString data.start.file

                                else
                                    ""

                            else if 1 == List.length (List.filter (.rank >> (==) data.start.rank) casesToDisambiguate) then
                                Square.rankToString data.start.rank

                            else
                                Square.toString data.start

                takesString =
                    case data.takes of
                        Nothing ->
                            ""

                        Just _ ->
                            "x"

                destinationString =
                    Square.toString data.end

                promotionString =
                    Maybe.map (.kind >> Piece.pieceKindToString >> String.toUpper >> (\x -> "=" ++ x)) data.promotion
                        |> Maybe.withDefault ""
            in
            pieceString ++ context ++ takesString ++ destinationString ++ promotionString

        Ply.EnPassant data ->
            [ Square.fileToString data.start.file, "x", Square.toString data.end ]
                |> String.join ""

        Ply.QueensideCastle _ ->
            "O-O-O"

        Ply.KingsideCastle _ ->
            "O-O"


toPgnHelp : Ply -> ( Position, List String ) -> ( Position, List String )
toPgnHelp ply ( position, strings ) =
    let
        plyText =
            plyToString position ply

        nextPosition =
            makeMove position ply
                |> Maybe.withDefault initial

        moveNumber =
            if position.playerToMove == Player.WhitePlayer then
                History.moveNumber nextPosition.history
                    |> String.fromInt
                    |> (\i -> i ++ ". ")

            else
                ""
    in
    ( nextPosition, (moveNumber ++ plyText) :: strings )


toPgn : Position -> String
toPgn position =
    List.foldl toPgnHelp ( initial, [] ) (History.toList position.history)
        |> Tuple.second
        |> List.reverse
        |> String.join " "


aiMove : Position -> Int -> Position
aiMove position seed =
    let
        allOptions =
            generateAllMovesForCurrentPlayerWithoutCheck position
                |> EverySet.toList
                |> List.sortBy (plyScore position)
                |> List.reverse

        selectedMoveIndex =
            modBy (List.length allOptions) seed

        selectedMove =
            List.drop (selectedMoveIndex - 1) allOptions |> List.head
    in
    case selectedMove of
        Nothing ->
            position

        Just m ->
            makeMove position m
                |> Maybe.withDefault position


plyScore : Position -> Ply -> Int
plyScore position ply =
    let
        speculativePosition =
            makeMove position ply
    in
    case speculativePosition of
        Nothing ->
            0

        Just nextPosition ->
            case ply of
                Ply.KingsideCastle _ ->
                    5

                Ply.QueensideCastle _ ->
                    6

                Ply.EnPassant data ->
                    4

                Ply.StandardMove data ->
                    let
                        promotes =
                            case data.promotion of
                                Nothing ->
                                    0

                                Just _ ->
                                    10

                        takes =
                            case data.takes of
                                Nothing ->
                                    0

                                Just piece ->
                                    1 + Piece.value piece.kind

                        recentlyUsed =
                            History.getPlayerMoves position.playerToMove position.history
                                |> List.reverse
                                |> List.head
                                |> (\maybeP ->
                                        case maybeP of
                                            Nothing ->
                                                0

                                            Just p ->
                                                if Ply.getEnd p == Just (Ply.getStart ply) then
                                                    -3

                                                else
                                                    0
                                   )

                        pieceModifier =
                            case data.piece.kind of
                                Piece.King ->
                                    if History.hasKingMoved position.playerToMove position.history then
                                        -5

                                    else
                                        -7

                                Piece.Queen ->
                                    -2

                                Piece.Pawn ->
                                    let
                                        rankMod =
                                            if data.end.rank - data.start.rank > 1 then
                                                1

                                            else if data.end.rank == Player.lastRank position.playerToMove then
                                                5

                                            else
                                                0

                                        fileMod =
                                            if data.end.file == 3 || data.end.file == 4 then
                                                1

                                            else
                                                -1
                                    in
                                    rankMod + fileMod

                                _ ->
                                    0

                        threatMod =
                            let
                                threat =
                                    getSquareThreat nextPosition data.end
                            in
                            if threat >= 1 then
                                -1 * Piece.value data.piece.kind

                            else
                                0

                        totalThreat =
                            -1 * totalThreatValue nextPosition

                        checkModifier =
                            case speculativePosition of
                                Nothing ->
                                    0

                                Just pos ->
                                    if isCurrentPlayerInCheckMate pos then
                                        1000000000

                                    else if isPlayerInCheck pos.playerToMove pos then
                                        1

                                    else
                                        0
                    in
                    promotes + takes + pieceModifier + checkModifier + threatMod + totalThreat + recentlyUsed


getSquareThreat position square =
    generateAllMovesForCurrentPlayerWithoutCheck position
        |> EverySet.filter (\p -> Ply.getEnd p == Just square)
        |> EverySet.size


totalThreatValue position =
    let
        otherPlayerAttacks =
            generateAllMovesForCurrentPlayerWithoutCheck position
                |> EverySet.map Ply.getEnd
                |> EverySet.toList
                |> List.filterMap identity
                |> List.filterMap (get position)
                |> List.map .kind
                |> List.map Piece.value
                |> List.sum

        currentPlayerAttacks =
            generateAllMovesForCurrentPlayerWithoutCheck { position | playerToMove = Player.otherPlayer position.playerToMove }
                |> EverySet.map Ply.getEnd
                |> EverySet.toList
                |> List.filterMap identity
                |> List.filterMap (get position)
                |> List.map .kind
                |> List.map Piece.value
                |> List.sum
    in
    otherPlayerAttacks - currentPlayerAttacks
