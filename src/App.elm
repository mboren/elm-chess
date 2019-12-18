module App exposing (..)

import Array2D exposing (Array2D)
import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import EverySet exposing (EverySet)
import History
import Html exposing (Html)
import Piece exposing (Piece, toString)
import Player exposing (Player(..))
import Position exposing (Position)
import Square exposing (File, Rank, Square)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type GameStatus
    = SelectingPiece
    | SelectingMove Square (EverySet Square)


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
    | MoveTo Square


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPiece square ->
            let
                squareContent =
                    Position.get model.position square
            in
            { model | status = SelectingMove square (Position.getPossibleMovesForCurrentPlayerWithoutCheck model.position square) }

        MoveTo square ->
            case model.status of
                SelectingMove start possibleMoves ->
                    let
                        newPosition =
                            Position.makeMove model.position start square
                    in
                    case newPosition of
                        Nothing ->
                            model

                        Just pos ->
                            { model | position = pos, status = SelectingPiece }

                _ ->
                    model



--MoveTo square ->
--    model
-- VIEW


rowToString : List (Maybe Piece) -> String
rowToString row =
    row |> List.map (Maybe.map toString) |> List.map (Maybe.withDefault "#") |> String.join ","


view : Model -> Html Msg
view model =
    Element.layout
        []
        (Element.column
            []
            [ drawBoard model
            , drawHistory model.position
            , drawStatus model
            ]
        )


drawHistory : Position -> Element Msg
drawHistory position =
    History.toStrings position.history |> String.join " " |> Element.text


drawStatus : Model -> Element Msg
drawStatus model =
    let
        boolToString b =
            if b then
                "T"

            else
                "F"
    in
    {- this function is used for misc stuff i want to see while developing -}
    [ ( "hasKingMoved (white)", History.hasKingMoved WhitePlayer model.position.history )
    , ( "hasKingMoved (black)", History.hasKingMoved BlackPlayer model.position.history )
    , ( "hasQueensideRookMoved (white)", History.hasQueensideRookMoved WhitePlayer model.position.history )
    , ( "hasQueensideRookMoved (black)", History.hasQueensideRookMoved BlackPlayer model.position.history )
    , ( "canKingsideCastle", Position.canKingsideCastle model.position )
    , ( "canQueensideCastle", Position.canQueensideCastle model.position )
    , ( "inCheck (white)", Position.isPlayerInCheck WhitePlayer model.position )
    , ( "inCheck (black)", Position.isPlayerInCheck BlackPlayer model.position )
    ]
        |> List.map (\( text, flag ) -> text ++ ": " ++ boolToString flag)
        |> List.map Element.text
        |> Element.column []


drawBoard : Model -> Element Msg
drawBoard model =
    let
        currentPlayersSquares =
            Position.getSquaresOccupiedByCurrentPlayer model.position

        selectedSquare =
            case model.status of
                SelectingPiece ->
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
            Array2D.indexedMap (squareEl (Element.px 50) currentPlayersSquares possibleMoves selectedSquare) model.position.board |> Position.getRows

        rows =
            List.map (Element.row []) foo
    in
    Element.column
        [ Element.width Element.fill ]
        rows


squareColor : Maybe Square -> EverySet Square -> Square -> Element.Color
squareColor selectedSquare possibleMoves currentSquare =
    let
        isPossibleMove =
            EverySet.member currentSquare possibleMoves

        greenOffset =
            case selectedSquare of
                Nothing ->
                    0

                Just sq ->
                    if sq.rank == currentSquare.rank && sq.file == currentSquare.file then
                        -50

                    else
                        0

        whiteSquareColor =
            Element.rgb255 255 (241 + greenOffset) 173

        blackSquareColor =
            Element.rgb255 0 (150 + greenOffset) 53
    in
    if isPossibleMove then
        Element.rgb255 255 0 0

    else
    -- if the evenness of the rank and file are the same, then it is black, otherwise it is white
    -- eg file A (=1), rank 8 is white, file B (=2), rank 8 is black
    if
        modBy 2 (currentSquare.rank + 1) == modBy 2 (currentSquare.file + 1)
    then
        blackSquareColor

    else
        whiteSquareColor


squareEl : Element.Length -> EverySet Square -> EverySet Square -> Maybe Square -> Rank -> File -> Maybe Piece -> Element Msg
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

        squareClickEvent =
            if EverySet.member (Square rank file) selectablePieceSquares then
                [ Element.Events.onClick (SelectPiece (Square rank file)) ]

            else if EverySet.member (Square rank file) possibleMoves then
                [ Element.Events.onClick (MoveTo (Square rank file)) ]

            else
                []
    in
    Element.el
        ([ Background.color (squareColor selectedSquare possibleMoves (Square rank file))
         , Font.color mainFontColor
         , Font.center
         , Font.glow (Element.rgb 0 0 0) 1.0
         , Element.padding 0
         , Element.width size
         , Element.height size
         ]
            ++ squareClickEvent
        )
        (case maybePiece of
            Nothing ->
                Element.none

            Just p ->
                Element.text (Piece.toString p)
        )
