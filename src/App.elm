module App exposing (..)

import Array2D exposing (Array2D)
import Browser exposing (Document)
import Browser.Events
import Element exposing (Element)
import Element.Background as Background
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input
import EverySet exposing (EverySet)
import History
import Piece exposing (Piece, PieceKind(..))
import Player exposing (Player(..))
import Ply exposing (Ply(..))
import Position exposing (Position)
import Square exposing (File, Rank, Square)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias BoardRenderInfo =
    { squareSize : Element.Length
    , selectablePieceSquares : EverySet Square
    , possiblePlies : EverySet Ply
    , selectedSquare : Maybe Square
    , lastPly : Maybe Ply
    }


type GameStatus
    = SelectingPiece
    | SelectingMove Square (EverySet Ply)
    | Checkmate
    | Draw
    | TimeWin Player


type TimeControl
    = None
    | Static
    | Increment Float


type alias Timer =
    { whiteTime : Float
    , blackTime : Float
    }


updateTimer : Timer -> Player -> Float -> Timer
updateTimer timer player delta =
    case player of
        Player.White ->
            { timer | whiteTime = max 0 (timer.whiteTime - delta) }

        Player.Black ->
            { timer | blackTime = max 0 (timer.blackTime - delta) }


timeRemaining timer player =
    case player of
        Player.White ->
            timer.whiteTime

        Player.Black ->
            timer.blackTime


handleIncrement : Model -> Model
handleIncrement model =
    case model.timeControl of
        Increment inc ->
            { model | timer = updateTimer model.timer (Player.otherPlayer model.position.playerToMove) (-1 * inc) }

        _ ->
            model


type alias Model =
    { position : Position
    , status : GameStatus
    , pgnInput : String
    , pgnParsingError : Maybe String
    , timer : Timer
    , timeControl : TimeControl
    , helpTextVisible : Bool
    }


init : Model
init =
    { position = Position.initial
    , status = SelectingPiece
    , pgnInput = ""
    , pgnParsingError = Nothing
    , timer = { whiteTime = 5.0 * 60.0 * 1000.0, blackTime = 5.0 * 60.0 * 1000.0 }
    , timeControl = None
    , helpTextVisible = True
    }



-- UPDATE


type Msg
    = SelectPiece Square
    | MoveTo Ply
    | AiMove
    | DebugLogPosition
    | UpdatePgnInput String
    | Tick Float
    | ToggleHelpText


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPiece square ->
            ( { model | status = SelectingMove square (Position.getPossibleMovesForCurrentPlayerWithoutCheck model.position square) }, Cmd.none )

        AiMove ->
            case model.status of
                SelectingPiece ->
                    let
                        newPosition =
                            Position.aiMove model.position 0

                        status =
                            if Position.isCurrentPlayerInCheckMate newPosition then
                                Checkmate
                            else if Position.isStalemate newPosition then
                                Draw

                            else
                                SelectingPiece
                    in
                    ( handleIncrement { model | position = newPosition, status = status }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MoveTo ply ->
            let
                newModel =
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
                                    else if Position.isStalemate pos then
                                        { model | position = pos, status = Draw }
                                    else
                                        handleIncrement { model | position = pos, status = SelectingPiece }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        UpdatePgnInput text ->
            case Position.fromPgn text of
                Ok newPosition ->
                    ( { model | pgnInput = text, pgnParsingError = Nothing, position = newPosition }, Cmd.none )

                Err error ->
                    ( { model | pgnInput = text, pgnParsingError = Just error }, Cmd.none )

        Tick delta ->
            let
                newTimer =
                    updateTimer model.timer model.position.playerToMove delta

                status =
                    if timeRemaining newTimer Player.White <= 0 then
                        TimeWin Player.Black

                    else if timeRemaining newTimer Player.Black <= 0 then
                        TimeWin Player.White

                    else
                        model.status
            in
            ( { model | timer = newTimer, status = status }, Cmd.none )

        ToggleHelpText ->
            ( { model | helpTextVisible = not model.helpTextVisible }, Cmd.none )

        DebugLogPosition ->
            let
                _ =
                    Debug.log "" model.position
            in
            ( model, Cmd.none )



-- SUBSCRIPTIONS


isGameOver status =
    case status of
        SelectingPiece ->
            False

        SelectingMove _ _ ->
            False

        Checkmate ->
            True

        Draw ->
            True

        TimeWin _ ->
            True


subscriptions model =
    if isGameOver model.status || model.timeControl == None then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta Tick



-- VIEW


view : Model -> Document Msg
view model =
    { title = "chess"
    , body =
        [ Element.layout
            []
            (Element.column
                []
                [ drawHelpText model
                , drawTakenPieces (History.getTakenPieces Black model.position.history)
                , drawBoard model
                , drawTimer model
                , drawTakenPieces (History.getTakenPieces White model.position.history)
                , drawHistory model.position
                , drawStatus model
                , drawButton DebugLogPosition "Log position"
                , drawButton AiMove "ai move"
                , drawDebugInfo model
                , drawPgnParsingAutoTestResults model.position
                , drawPgnInput model
                ]
            )
        ]
    }


drawTimer : Model -> Element Msg
drawTimer model =
    if model.timeControl == None then
        Element.none

    else
        let
            toStringWithLeadingZero num =
                if num < 10 then
                    "0" ++ String.fromInt num

                else
                    String.fromInt num

            millisToString millis =
                let
                    hour =
                        60 * 60 * 1000

                    minute =
                        60 * 1000

                    second =
                        1000

                    hours =
                        floor millis // hour

                    minutes =
                        (floor millis - (hour * hours)) // minute

                    seconds =
                        (floor millis - (hour * hours) - (minute * minutes)) // second
                in
                if hours > 0 then
                    [ String.fromInt hours, toStringWithLeadingZero minutes, toStringWithLeadingZero seconds ] |> String.join ":"

                else if minutes > 0 then
                    [ String.fromInt minutes, toStringWithLeadingZero seconds ] |> String.join ":"

                else
                    String.fromInt seconds
        in
        Element.row [ Element.spacing 10 ]
            [ Element.el [ Element.Border.color (Element.rgb255 0 0 0) ] (Element.text (millisToString model.timer.whiteTime))
            , Element.el [] (Element.text (millisToString model.timer.blackTime))
            ]


drawPgnInput : Model -> Element Msg
drawPgnInput model =
    let
        ( parsingStatusText, statusColor ) =
            case model.pgnParsingError of
                Nothing ->
                    ( "Valid :)", Element.rgb255 0 200 0 )

                Just error ->
                    ( error, Element.rgb255 200 0 0 )
    in
    Element.column []
        [ Element.Input.multiline
            []
            { onChange = UpdatePgnInput
            , text = model.pgnInput
            , placeholder = Nothing
            , label = Element.Input.labelAbove [] (Element.text "Enter a new position in PGN")
            , spellcheck = False
            }
        , Element.el [ Font.color statusColor ] (Element.text parsingStatusText)
        ]


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

                TimeWin player ->
                    Player.toString player ++ " wins!"

                Draw ->
                    "Stalemate"

                _ ->
                    (Player.toString <| model.position.playerToMove) ++ " to move"
    in
    Element.text text


drawHistory : Position -> Element Msg
drawHistory position =
    Element.text (Position.toPgn position)


drawPgnParsingAutoTestResults : Position -> Element Msg
drawPgnParsingAutoTestResults position =
    let
        pgnText =
            Position.toPgn position

        pgnParsingResult =
            Position.fromPgn pgnText

        resultText =
            case pgnParsingResult of
                Ok pos ->
                    if pos == position then
                        "Ok"

                    else
                        let
                            parsedPosition =
                                Debug.log "Parsed position" pos

                            currentPosition =
                                Debug.log "Actual position" position
                        in
                        "Parsed position does not match current position! See console for details."

                Err err ->
                    err
    in
    Element.text resultText


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

                Draw ->
                    EverySet.empty

                TimeWin _ ->
                    EverySet.empty

                _ ->
                    Position.getSquaresOccupiedByCurrentPlayer model.position

        ( selectedSquare, possibleMoves ) =
            case model.status of
                SelectingPiece ->
                    ( Nothing, EverySet.empty )

                Checkmate ->
                    ( Nothing, EverySet.empty )

                Draw ->
                    ( Nothing, EverySet.empty )

                SelectingMove square plies ->
                    ( Just square, plies )

                TimeWin _ ->
                    ( Nothing, EverySet.empty )

        renderInfo =
            { squareSize = Element.px 45
            , selectablePieceSquares = currentPlayersSquares
            , possiblePlies = possibleMoves
            , selectedSquare = selectedSquare
            , lastPly = History.getLastPly model.position.history
            }
    in
    Element.column
        [ Element.width Element.fill ]
        (Array2D.indexedMap (squareEl renderInfo) model.position.board
            |> Position.getRows
            |> orientBoard model.position.playerToMove
            |> List.map (Element.row [])
        )


orientBoard : Player -> List (List a) -> List (List a)
orientBoard player board =
    case player of
        Player.White ->
            board

        Player.Black ->
            List.reverse board
                |> List.map List.reverse


squareColor : BoardRenderInfo -> Square -> Element.Color
squareColor { selectedSquare, lastPly } currentSquare =
    let
        normalColors =
            ( Element.rgb255 237 238 210, Element.rgb255 0 150 53 )

        selectedColors =
            ( Element.rgb255 255 241 0, Element.rgb255 255 241 0 )

        lastPlyColors =
            ( Element.rgb255 233 255 92, Element.rgb255 120 223 51 )

        unselectedColors =
            case lastPly of
                Nothing ->
                    normalColors

                Just lp ->
                    if Ply.getStart lp == currentSquare || Ply.getEnd lp == currentSquare then
                        lastPlyColors

                    else
                        normalColors

        ( whiteSquareColor, blackSquareColor ) =
            case selectedSquare of
                Nothing ->
                    unselectedColors

                Just sq ->
                    if sq == currentSquare then
                        selectedColors

                    else
                        unselectedColors
    in
    -- if the evenness of the rank and file are the same, then it is black, otherwise it is white
    -- eg file A (=1), rank 8 is white, file B (=2), rank 8 is black
    if modBy 2 currentSquare.rank == modBy 2 currentSquare.file then
        blackSquareColor

    else
        whiteSquareColor


drawPiece : Maybe Piece -> Element Msg
drawPiece maybePiece =
    case maybePiece of
        Nothing ->
            Element.none

        Just p ->
            Element.image [] { src = pieceToFileName p, description = Piece.toString p }


possibleMoveOverlay =
    Element.el [ Element.Border.rounded 50, Background.color (Element.rgb255 255 0 0), Element.width Element.fill, Element.height Element.fill, Element.alpha 0.5 ] Element.none


squareEl : BoardRenderInfo -> Rank -> File -> Maybe Piece -> Element Msg
squareEl renderInfo rank file maybePiece =
    let
        currentSquare =
            Square rank file

        moveSquares =
            EverySet.map Ply.toSquareForMoveSelection renderInfo.possiblePlies

        squareClickEvent =
            if EverySet.member currentSquare renderInfo.selectablePieceSquares then
                [ Element.Events.onClick (SelectPiece currentSquare) ]

            else if EverySet.member currentSquare moveSquares then
                case Ply.getMoveAssociatedWithSquare (EverySet.toList renderInfo.possiblePlies) currentSquare of
                    Nothing ->
                        []

                    Just ply ->
                        [ Element.Events.onClick (MoveTo ply) ]

            else
                []

        overlay =
            if EverySet.member currentSquare moveSquares then
                possibleMoveOverlay

            else
                Element.none

        backgroundColor =
            squareColor renderInfo currentSquare
    in
    Element.el
        ([ Background.color backgroundColor
         , Element.padding 0
         , Element.width renderInfo.squareSize
         , Element.height renderInfo.squareSize
         , Element.inFront overlay
         ]
            ++ squareClickEvent
        )
        (drawPiece maybePiece)


pieceToFileName piece =
    let
        color =
            case piece.color of
                Player.White ->
                    "l"

                Player.Black ->
                    "d"

        pieceKind =
            String.toLower (Piece.pieceKindToString piece.kind)
    in
    "../images/Chess_" ++ pieceKind ++ color ++ "t45.svg"


drawButton msg text =
    Element.Input.button
        [ Background.color (Element.rgb255 128 128 128)
        , Element.Border.rounded 10
        , Element.Border.width 10
        , Element.Border.color (Element.rgb255 128 128 128)
        ]
        { onPress = Just msg, label = Element.text text }


drawHelpText model =
    if model.helpTextVisible then
        Element.column
            [ Element.width (Element.fill |> Element.maximum 500), Element.padding 5 ]
            [ drawButton ToggleHelpText "Hide help"
            , Element.textColumn [ Element.spacing 10, Element.padding 5, Element.Border.width 1 ]
                [ Element.paragraph [] [ Element.text "This is a basic hotseat multiplayer chess game." ]
                , Element.paragraph [] [ Element.text "The main goal of this project was to see what writing the logic for chess was like in the Elm programming language, so the UI is pretty rough." ]
                , Element.paragraph [] [ Element.text "Click a piece to see where it can move, then click on one of the red circles that appear to move it." ]
                , Element.paragraph [] [ Element.text "The board will flip around after every move." ]
                , Element.paragraph [] [ Element.text "To have the computer make a move for you, press the \"ai move\" button." ]
                , Element.paragraph [] [ Element.text "You can also enter a chess position by pasting the PGN movetext in the text box at the bottom. (comments are not supported)" ]
                ]
            ]

    else
        Element.column
            []
            [ drawButton ToggleHelpText "Show help"
            ]
