-- | TODO: Generalise the ideas here to a `Game` typeclass, which the minimax
-- | algorithm should then work with.
module Games.TicTacToe.Logic where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (all, fold, surround, surroundMap, oneOf, oneOfMap, maximumBy)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Index (Index)
import Data.Matrix (Matrix)
import Data.Matrix as Matrix
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Newtype (class Newtype, unwrap, over)
import Data.Ord.Max (Max(Max))
import Data.Ord.Min (Min(Min))
import Data.Rose (Rose, (:>))
import Data.Rose as Rose
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
import Data.Tuple as Tuple
import Data.TypeLevel.Nat (Three)
import Data.Vec (Vec)
import Data.Vec as Vec


-- | Two player game, X and O.
data Player = P1 | P2

derive instance eqPlayer :: Eq Player

instance showPlayer :: Show Player where
    show P1 = "X"
    show P2 = "O"

-- | Switch to the other player.
otherPlayer :: Player -> Player
otherPlayer P1 = P2
otherPlayer P2 = P1


-- | 3x3 game board.
newtype Board = Board (Matrix Three Three (Maybe Player))

derive instance newtypeBoard :: Newtype Board _

instance showBoard :: Show Board where
    show (Board board) =
        surround ("\n" <> hr <> "\n")
            (map (surroundMap "|" (maybe " " show)) board)
      where
        hr :: String
        hr = fold (Array.replicate (3 * 2 + 1) "-")


-- | Start a new game.
new :: Board
new  = Board (Vec.fill (Vec.fill Nothing))


-- Pick the winner from a board (if there is one).
winner :: Board -> Maybe Player
winner (Board board) = oneOf
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


draw :: Board -> Boolean
draw = unwrap >>> all (all isJust)


-- | TODO: This should randomly select a move if there is no single maximum.
nextMove :: Int -> Player -> Board -> Maybe Board
nextMove maxDepth player board =
    Tuple.snd <$> maximumBy (comparing Tuple.fst) (Array.zip scores moves)
  where
    tree :: Rose Board
    tree = gameTree maxDepth player board

    scores :: Array Int
    scores = map Rose.root (Rose.firstBranch (minimax maxDepth player tree))

    moves :: Array Board
    moves = map Rose.root (Rose.firstBranch tree)


gameTree :: Int -> Player -> Board -> Rose Board
gameTree maxDepth player board
    | maxDepth <= 0 = Rose.leaf board
    | otherwise     = board :>
        map (gameTree (maxDepth - 1) (otherPlayer player)) availableMoves
  where
    availableMoves :: Array Board
    availableMoves =
        foldMapWithIndex
        (\i -> Array.catMaybes <<< Vec.toArray <<< mapWithIndex (move i))
        (unwrap board)

    move :: Index Three -> Index Three -> Maybe Player -> Maybe Board
    move _ _ (Just _) = Nothing
    move i j Nothing  = Just (makeMove { i, j } player board)


makeMove :: { i :: Index Three, j :: Index Three } -> Player -> Board -> Board
makeMove { i, j } player =
    over Board (Vec.modifyAt i (Vec.updateAt j (Just player)))


minimax :: Int -> Player -> Rose Board -> Rose Int
minimax = minimax' 0


minimax' :: Int -> Int -> Player -> Rose Board -> Rose Int
minimax' depth maxDepth player (board :> boards)
    | depth >= maxDepth = finalScore (maxDepth - depth) board
    | otherwise =
    case NonEmptyArray.fromArray boards of
         -- No more moves left
         Nothing -> finalScore (maxDepth - depth) board

         -- Pick a move
         Just boards' ->
             case player of
                  -- maximising P1's score
                  P1 -> let scores = map (minimax' (depth - 1) maxDepth P2) boards'
                         in maximum1 (map Rose.root scores) :> NonEmptyArray.toArray scores
                  -- minimising P2's score
                  P2 -> let scores = map (minimax' (depth - 1) maxDepth P1) boards'
                         in minimum1 (map Rose.root scores) :> NonEmptyArray.toArray scores


-- | Scoring logic (with weighting).
finalScore :: Int -> Board -> Rose Int
finalScore weight board =
    case winner board of
         Just P1 -> Rose.leaf (1    * weight)
         Just P2 -> Rose.leaf ((-1) * weight)
         Nothing -> Rose.leaf 0


-- | Maximum of a non empty Foldable.
maximum1 :: forall a f. Ord a => Foldable1 f => f a -> a
maximum1 = unwrap <<< foldMap1 Max


-- | Maximum of a non empty Foldable.
minimum1 :: forall a f. Ord a => Foldable1 f => f a -> a
minimum1 = unwrap <<< foldMap1 Min
