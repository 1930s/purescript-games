module Games.TicTacToe.Logic
    ( gameSpec
    , availableMoves
    , switchPlayer
    , result
    , Player
    , X(X)
    , O(O)
    , showPlayer
    , Board
    , Position
    , Result
    , newGame
    , draw
    , makeMove
    )
where

import Prelude
import Data.Array as Array
import Data.Either (Either(Left, Right), either)
import Data.Foldable (all, fold, oneOfMap, surround, surroundMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Game (GameSpec)
import Data.Game as Game
import Data.Index (Index)
import Data.Matrix (Matrix)
import Data.Matrix as Matrix
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Newtype (class Newtype, wrap, unwrap, over)
import Data.TypeLevel.Nat (Three)
import Data.Vec (Vec)
import Data.Vec as Vec


-- | Player one.
data X = X

instance showP1 :: Show X where show _ = "X"
instance eqP1 :: Eq X where eq _ _ = true


-- | Player two.
data O = O

instance showP2 :: Show O where show _ = "0"
instance eqP2 :: Eq O where eq _ _ = true


-- | Two player game.
type Player = Either X O


showPlayer :: Player -> String
showPlayer = either show show


type Result = Game.Result X O


-- | Change players.
switchPlayer :: Player -> Player
switchPlayer (Left X)  = Right O
switchPlayer (Right O) = Left X


-- | 3x3 game board.
newtype Board = Board (Matrix Three Three (Maybe Player))

derive instance newtypeBoard :: Newtype Board _

instance showBoard :: Show Board where
    show (Board board) =
        surround ("\n" <> hr <> "\n")
            (map (surroundMap "|" (maybe " " showPlayer)) board)
      where
        hr :: String
        hr = fold (Array.replicate (3 * 2 + 1) "-")


-- | Start a new game.
newGame :: Board
newGame  = Board (Vec.fill (Vec.fill Nothing))


-- | Position on the `Board`.
type Position = { i :: Index Three, j :: Index Three }


-- | Description of the game.
gameSpec :: GameSpec Board X O
gameSpec =
    { availableMoves
    , switchPlayer
    , result
    }


-- | Pick the winner from a board (if there is one).
result :: Board -> Maybe Result
result (Board board)
    | draw (wrap board) = Just Game.Draw
    | otherwise = oneOfMap (map Game.Winner)
        [ -- Row winner
          oneOfMap findWinner (Matrix.rows board)
          -- Column winner
        , oneOfMap findWinner (Matrix.cols board)
          -- Diagonal winner
        , findWinner (Vec.diag  board)
        , findWinner (Vec.diag' board)
        ]
  where
    findWinner :: Vec Three (Maybe Player) -> Maybe Player
    findWinner = Vec.foldl1 (\p p' -> if p == p' then p else Nothing)


-- | Is it a draw?
draw :: Board -> Boolean
draw = unwrap >>> all (all isJust)


makeMove :: Position -> Player -> Board -> Board
makeMove { i, j } player =
    over Board (Vec.modifyAt i (Vec.updateAt j (Just player)))


availableMoves :: Player -> Board -> Array Board
availableMoves player board =
    foldMapWithIndex
    (\i -> Array.catMaybes <<< Vec.toArray <<< mapWithIndex (move i))
    (unwrap board)
  where
    move :: Index Three -> Index Three -> Maybe Player -> Maybe Board
    move _ _ (Just _) = Nothing
    move i j Nothing  = Just (makeMove { i, j } player board)
