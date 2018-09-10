module Games.TicTacToe.UI
    ( main
    )
where

import Prelude
import Data.Const (Const)
import Data.Either (Either(Right, Left))
import Data.Functor.Coproduct (Coproduct)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Game (Result(Winner, Draw))
import Data.Game as Game
import Data.Index (Index)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (unwrap)
import Data.TypeLevel.Nat (Three)
import Data.Vec (Vec)
import Data.Vec as Vec
import Effect (Effect)
import Games.TicTacToe.Logic as TicTacToe
import Spork.App (App)
import Spork.App as App
import Spork.Html (Html)
import Spork.Html as H
import Spork.Interpreter (Interpreter)
import Spork.Interpreter as Interpreter
import Spork.Transition (Transition)


type Model =
    { board      :: TicTacToe.Board
    , difficulty :: Int
    , busy       :: Boolean
    , result     :: Maybe TicTacToe.Result
    }


initialModel :: Model
initialModel =
    { board: TicTacToe.newGame
    , difficulty: 5
    , busy: false
    , result: Nothing
    }


user :: TicTacToe.Player
user = Left TicTacToe.X


computer :: TicTacToe.Player
computer = Right TicTacToe.O


data Action
    = Reset
    | SetDifficulty String
    | UserMove TicTacToe.Position TicTacToe.Player
    | ComputerMoved (Maybe TicTacToe.Board)


data GameEffect a
    = ComputerMove Model (Maybe TicTacToe.Board -> a)


runGameEffect :: GameEffect ~> Effect
runGameEffect = case _ of
    ComputerMove { board, difficulty: maxDepth } reply ->
        reply <$> Game.nextMove TicTacToe.gameSpec maxDepth computer board


render :: Model -> Html Action
render { board, result, difficulty, busy } =
    H.div []
        [ H.div []
            [ if busy
                 then H.text "Thinking..."
                 else case result of
                            Nothing -> H.text ""
                            Just Draw -> H.text "It's a draw"
                            Just (Winner winner) ->
                                H.text (TicTacToe.showPlayer winner <> " wins!")
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
                , H.onClick (H.always_ (UserMove { i, j } user))
                ]
                [ H.text "" ]

        Just player ->
            H.div
                [ H.classes
                    [ "Board__cell"
                    , "Board__cell--disabled"
                    ]
                ]
                [ H.text (TicTacToe.showPlayer player) ]


update :: Model -> Action -> Transition GameEffect Model Action
update model Reset =
    App.purely model { board = TicTacToe.newGame, result = Nothing }

update model (SetDifficulty str) =
    case Int.fromString str of
         Nothing -> App.purely model -- NoOp
         Just int
            | int > 0 && int <= 10 ->
                update model { difficulty = int } Reset
            | otherwise ->
                App.purely model

update model (UserMove position player)
    | model.busy          = App.purely model -- NoOp (busy)
    | isJust model.result = App.purely model -- NoOp (game over)
    | otherwise =
    let board' = TicTacToe.makeMove position player model.board in
    case TicTacToe.result board' of
            Just result -> App.purely model { board = board', result = Just result }
            Nothing ->
                let model' = model { board = board', busy = true } in
                { model: model'
                , effects: App.lift (ComputerMove model' ComputerMoved)
                }

update model (ComputerMoved Nothing) = App.purely model -- wtf
update model (ComputerMoved (Just board)) =
    App.purely model
        { board = board
        , result = TicTacToe.result board
        , busy = false
        }


app :: App GameEffect (Const Void) Model Action
app = { render, update, subs: const mempty, init: App.purely initialModel }


interpreter :: Interpreter Effect (Coproduct GameEffect (Const Void)) Action
interpreter = Interpreter.liftNat runGameEffect `Interpreter.merge` Interpreter.never


main :: Effect Unit
main = do
    inst <- App.makeWithSelector interpreter app "#app"
    inst.run
