module App exposing (..)

import Array2D exposing (Array2D)
import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input
import EverySet exposing (EverySet)
import History
import Html exposing (Html)
import Piece exposing (Piece)
import Player exposing (Player(..))
import Ply exposing (Ply, toSquareForMoveSelection)
import Position exposing (Position)
import Square exposing (File, Rank, Square)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type GameStatus
    = SelectingPiece
    | SelectingMove Square (EverySet Ply)
    | Checkmate


type alias Model =
    { position : Position
    , status : GameStatus
    }


init : Model
init =
    { position = Position.initial
    , status = SelectingPiece
    }



-- UPDATE


type Msg
    = SelectPiece Square
    | MoveTo Ply
    | DebugLogPosition


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPiece square ->
            { model | status = SelectingMove square (Position.getPossibleMovesForCurrentPlayerWithoutCheck model.position square) }

        MoveTo ply ->
            case model.status of
                SelectingMove start _ ->
                    let
                        newPosition =
                            Position.makeMove model.position ply
                    in
                    case newPosition of
                        Nothing ->
                            model

                        Just pos ->
                            if Position.isCurrentPlayerInCheckMate pos then
                                { model | position = pos, status = Checkmate }

                            else
                                { model | position = pos, status = SelectingPiece }

                _ ->
                    model

        DebugLogPosition ->
            let
                _ =
                    Debug.log "" model.position
            in
            model



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        []
        (Element.column
            []
            [ drawTakenPieces (History.getTakenPieces BlackPlayer model.position.history)
            , drawBoard model
            , drawTakenPieces (History.getTakenPieces WhitePlayer model.position.history)
            , drawHistory model.position
            , drawStatus model
            , Element.Input.button [ Background.color (Element.rgb255 128 128 128), Element.Border.rounded 10, Element.Border.width 10, Element.Border.color (Element.rgb255 128 128 128) ] { onPress = Just DebugLogPosition, label = Element.text "Log position" }
            , drawDebugInfo model
            ]
        )


drawTakenPieces : List Piece -> Element Msg
drawTakenPieces pieces =
    pieces |> List.map Piece.toString |> String.join "" |> Element.text


drawStatus : Model -> Element Msg
drawStatus model =
    let
        text =
            case model.status of
                Checkmate ->
                    (Player.toString <| Player.otherPlayer <| model.position.playerToMove) ++ " wins!"

                _ ->
                    (Player.toString <| model.position.playerToMove) ++ " to move"
    in
    Element.text text


drawHistory : Position -> Element Msg
drawHistory position =
    History.toStrings position.history |> String.join " " |> Element.text


drawDebugInfo : Model -> Element Msg
drawDebugInfo model =
    let
        boolToString b =
            if b then
                "T"

            else
                "F"
    in
    {- this function is used for misc stuff i want to see while developing -}
    -- (label string, bool expression)
    []
        |> List.map (\( text, flag ) -> text ++ ": " ++ boolToString flag)
        |> List.map Element.text
        |> Element.column []


drawBoard : Model -> Element Msg
drawBoard model =
    let
        currentPlayersSquares =
            case model.status of
                Checkmate ->
                    EverySet.empty

                _ ->
                    Position.getSquaresOccupiedByCurrentPlayer model.position

        selectedSquare =
            case model.status of
                SelectingPiece ->
                    Nothing

                Checkmate ->
                    Nothing

                SelectingMove square _ ->
                    Just square

        possibleMoves =
            case selectedSquare of
                Nothing ->
                    EverySet.empty

                Just sq ->
                    Position.getPossibleMovesForCurrentPlayerWithoutCheck model.position sq

        -- TODO foo
        foo : List (List (Element Msg))
        foo =
            Array2D.indexedMap (squareEl (Element.px 50) currentPlayersSquares possibleMoves selectedSquare) model.position.board
                |> Position.getRows

        rows =
            List.map (Element.row []) foo
    in
    Element.column
        [ Element.width Element.fill ]
        rows


squareColor : Maybe Square -> Square -> Element.Color
squareColor selectedSquare currentSquare =
    let
        unselectedColors =
            ( Element.rgb255 237 238 210, Element.rgb255 0 150 53 )

        selectedColors =
            ( Element.rgb255 255 241 0, Element.rgb255 255 241 0 )

        ( whiteSquareColor, blackSquareColor ) =
            case selectedSquare of
                Nothing ->
                    unselectedColors

                Just sq ->
                    if sq.rank == currentSquare.rank && sq.file == currentSquare.file then
                        selectedColors

                    else
                        unselectedColors
    in
    -- if the evenness of the rank and file are the same, then it is black, otherwise it is white
    -- eg file A (=1), rank 8 is white, file B (=2), rank 8 is black
    if modBy 2 (currentSquare.rank + 1) == modBy 2 (currentSquare.file + 1) then
        blackSquareColor

    else
        whiteSquareColor


squareEl : Element.Length -> EverySet Square -> EverySet Ply -> Maybe Square -> Rank -> File -> Maybe Piece -> Element Msg
squareEl size selectablePieceSquares possibleMoves selectedSquare rank file maybePiece =
    let
        mainFontColor =
            case maybePiece of
                Nothing ->
                    Element.rgb255 255 0 0

                Just p ->
                    case p.color of
                        BlackPlayer ->
                            Element.rgb255 0 0 0

                        WhitePlayer ->
                            Element.rgb255 255 255 255

        moveSquares =
            EverySet.map Ply.toSquareForMoveSelection possibleMoves

        squareClickEvent =
            if EverySet.member (Square rank file) selectablePieceSquares then
                [ Element.Events.onClick (SelectPiece (Square rank file)) ]

            else if EverySet.member (Square rank file) moveSquares then
                case Ply.getMoveAssociatedWithSquare (EverySet.toList possibleMoves) (Square rank file) of
                    Nothing ->
                        []

                    Just ply ->
                        [ Element.Events.onClick (MoveTo ply) ]

            else
                []

        overlay =
            if EverySet.member (Square rank file) moveSquares then
                Element.el [ Element.Border.rounded 50, Background.color (Element.rgb255 255 0 0), Element.width Element.fill, Element.height Element.fill, Element.alpha 0.5 ] Element.none

            else
                Element.none

        glow =
            case maybePiece of
                Nothing ->
                    []

                Just p ->
                    case p.color of
                        WhitePlayer ->
                            [ Font.glow (Element.rgb 0 0 0) 1.0 ]

                        BlackPlayer ->
                            []
    in
    Element.el
        ([ Background.color (squareColor selectedSquare (Square rank file))
         , Font.color mainFontColor
         , Font.size 50
         , Font.center
         , Element.padding 0
         , Element.width size
         , Element.height size
         , Element.inFront overlay
         ]
            ++ squareClickEvent
            ++ glow
        )
        (case maybePiece of
            Nothing ->
                Element.none

            Just p ->
                Element.text (Piece.toString p)
        )
