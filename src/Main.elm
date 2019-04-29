module Main exposing (Board, Player(..), initBoard)

import Array exposing (Array)
import Browser
import Element exposing (Element, centerX, column, el, fill, height, layout, minimum, none, padding, paddingXY, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html



{-
   https://discourse.elm-lang.org/t/tictactoe-should-the-winner-be-part-of-the-model/3519

      I’ve been trying to write a Tic Tac Toe game, and I couldn’t get my head around how to write a data model which would
      serve me best. Briefly, Tic Tac Toe consists of nine tiles grouped into three rows of three tiles. Two players
      interchangeably place crosses and circles on the board until either they’ve filled the board, or one of the players
      managed to put three symbols of the same type in a line. Henceforth it seems reasonable to deduce that

      the model should include nine tiles and the active player,
      and that we should somehow calculate whether the game is still in progress or has ended with a tie or one of the players
      has won.

      :bulb: What about the winner?
      I have seen a few examples of Tic Tac Toe which featured game state as a part of the model. I feel like featuring the
      “state” of the game in the model contradicts with the idea of making impossible states impossible since we could have a
      board which isn’t full yet and the “state” indicating that the game has ended with a tie (i.e. the board is full).

      Moreover, it doesn’t make much sense to have a “state” of the state.

      On the other hand, having a state of the game as part of the model could significantly improve performance. Or at least,
       it feels like it should enhance it since we wouldn’t be calculating the winner over and over again.
-}


type Msg
    = CellClick Int Int


type alias Model =
    { board : Board
    , currentPlayer : Player
    }


type Player
    = Blank
    | X
    | O


type alias Board =
    { rows : Array (Array Player)
    }


initBoard : Board
initBoard =
    { rows = Array.repeat 3 (Array.repeat 3 Blank)
    }


init : Model
init =
    { board = initBoard
    , currentPlayer = X
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellClick x y ->
            if model.currentPlayer == X then
                { model | currentPlayer = O, board = updateBoard model.board x y model.currentPlayer }

            else
                { model | currentPlayer = X, board = updateBoard model.board x y model.currentPlayer }


updateBoard : Board -> Int -> Int -> Player -> Board
updateBoard board x y p =
    let
        newRow =
            case Array.get y board.rows of
                Just aRow ->
                    Array.set x p aRow

                Nothing ->
                    Array.repeat 3 Blank
    in
    { board | rows = Array.set y newRow board.rows }


view : Model -> Html.Html Msg
view model =
    layout [ height fill ] <|
        column
            [ width fill
            , centerX
            , paddingXY 30 30
            , Font.size 16
            ]
            [ viewBoard model.board ]


viewBoard : Board -> Element Msg
viewBoard board =
    column []
        (List.indexedMap viewRow (Array.toList board.rows))


viewRow : Int -> Array Player -> Element Msg
viewRow x theRow =
    row []
        (List.indexedMap (viewCell x) (Array.toList theRow))


viewCell : Int -> Int -> Player -> Element Msg
viewCell y x player =
    column [ width (fill |> minimum 28) ]
        [ case player of
            Blank ->
                clickableCell x y "_"

            X ->
                nonClickableCell x y "X"

            O ->
                nonClickableCell x y "O"
        ]


clickableCell x y s =
    Input.button [ centerX, Border.width 1, padding 5 ]
        { onPress = Just (CellClick x y)
        , label = el [] <| text s
        }


nonClickableCell x y s =
    el [ centerX, Border.width 1, padding 5 ] <| text s


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
