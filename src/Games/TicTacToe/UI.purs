module Games.TicTacToe.UI where

import Prelude
import Data.FunctorWithIndex (mapWithIndex)
import Data.Index (Index)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (unwrap)
import Data.TypeLevel.Nat (Three)
import Data.Vec (Vec)
import Data.Vec as Vec
import Effect (Effect)
import Games.TicTacToe.Logic as TicTacToe
import Spork.Html (Html)
import Spork.Html as H
import Spork.PureApp (PureApp)
import Spork.PureApp as PureApp


data Result = Winner TicTacToe.Player | Draw


type Model =
    { board      :: TicTacToe.Board
    , difficulty :: Int
    , busy       :: Boolean
    , result     :: Maybe Result
    }


type Move = { i :: Index Three, j :: Index Three}


data Action
    = Reset
    | SetDifficulty String
    | MakeMove Move TicTacToe.Player


update :: Model -> Action -> Model
update model Reset =
    model { board = TicTacToe.new, result = Nothing }

update model (SetDifficulty str) =
    case Int.fromString str of
         Nothing -> model -- NoOp
         Just int
            | int > 0 && int <= 10 ->
                update model { difficulty = int } Reset
            | otherwise ->
                model

update model (MakeMove position player)
    | model.busy          = model -- NoOp (busy)
    | isJust model.result = model -- NoOp (game over)
    | otherwise =
    checkMove (TicTacToe.makeMove position player model.board) \board' ->
        case TicTacToe.nextMove model.difficulty (TicTacToe.otherPlayer player) board' of
             Nothing -> init -- wtf?
             Just board'' ->
                 checkMove board'' (model { board = _ })
  where
    checkMove :: TicTacToe.Board -> (TicTacToe.Board -> Model) -> Model
    checkMove board f =
        case TicTacToe.winner board of
             Just winner ->
                 model { board = board, result = Just (Winner winner) }
             Nothing
                | TicTacToe.draw board ->
                    model { board = board, result = Just Draw }
                | otherwise ->
                    f board


render :: Model -> Html Action
render { board, result, difficulty } =
    H.div []
        [ H.div []
            [ case result of
                   Nothing -> H.text ""
                   Just Draw -> H.text "It's a draw"
                   Just (Winner winner) -> H.text (show winner <> " wins!")
            ]
        , H.div [ H.classes [ "Board" ] ]
            (Vec.toArray <<< mapWithIndex renderRow <<< unwrap $ board)
        , H.button
            [ H.onClick (H.always_ Reset) ]
            [ H.text "Reset" ]
        , H.input
            [ H.type_ H.InputNumber
            , H.onValueInput (H.always SetDifficulty)
            , H.onValueChange (H.always SetDifficulty)
            , H.value (show difficulty)
            , H.attr "step" "1"
            , H.attr "min"  "1"
            , H.attr "max"  "9"
            ]
        ]
  where
    renderRow
        :: Index Three
        -> Vec Three (Maybe TicTacToe.Player)
        -> Html Action
    renderRow i row =
        H.div [ H.classes [ "Board__row" ] ]
            (Vec.toArray <<< mapWithIndex (renderPlayer i) $ row)

    renderPlayer
        :: Index Three
        -> Index Three
        -> Maybe TicTacToe.Player
        -> Html Action
    renderPlayer i j = case _ of
        Nothing ->
            H.div
                [ H.classes
                    [ "Board__cell"
                    , if isJust result then "Board__cell--disabled" else ""
                    ]
                , H.onClick (H.always_ (MakeMove { i, j } TicTacToe.P2 ))
                ]
                [ H.text "" ]

        Just player ->
            H.div
                [ H.classes
                    [ "Board__cell"
                    , "Board__cell--disabled"
                    ]
                ]
                [ case player of
                       TicTacToe.P1 -> H.text "X"
                       TicTacToe.P2 -> H.text "O"
                ]


init :: Model
init =
    { board: TicTacToe.new
    , difficulty: 5
    , busy: false
    , result: Nothing
    }


app :: PureApp Model Action
app = { update, render, init }


main ∷ Effect Unit
main = void (PureApp.makeWithSelector app "#app")
