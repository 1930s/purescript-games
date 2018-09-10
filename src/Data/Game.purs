module Data.Game
    ( GameSpec
    , Result(Winner, Draw)
    , nextMove
    )
where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(Left, Right))
import Data.Foldable (class Foldable, foldl, length)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Ord.Max (Max(Max))
import Data.Ord.Min (Min(Min))
import Data.Rose (Rose, (:>))
import Data.Rose as Rose
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Random (randomInt)


-- | Generic representation of a two-player, zero-sum game.
type GameSpec board p1 p2 =
    { availableMoves :: Either p1 p2 -> board -> Array board
    , switchPlayer   :: Either p1 p2 -> Either p1 p2
    , result         :: board -> Maybe (Result p1 p2)
    }


-- | A game results in either a single winner or a draw.
data Result p1 p2 = Winner (Either p1 p2) | Draw


-- | Choose the next move.
nextMove
    :: forall board p1 p2
     . GameSpec board p1 p2
    -> Int
    -> Either p1 p2
    -> board
    -> Effect (Maybe board)
nextMove spec maxDepth player board =
    case Tuple.snd <$> maximumsBy (comparing Tuple.fst) (Array.zip scores moves) of
         []         -> pure Nothing
         [ board' ] -> pure (Just board')
         boards     -> Array.index boards <$> randomInt 0 (length boards - 1)
  where
    tree :: Rose board
    tree = gameTree spec maxDepth player board

    scores :: Array Int
    scores = map Rose.root (Rose.firstBranch (minimax spec maxDepth player tree))

    moves :: Array board
    moves = map Rose.root (Rose.firstBranch tree)


-- | Construct a tree of possible game states.
gameTree
    :: forall board p1 p2
     . GameSpec board p1 p2   -- description of the game
    -> Int                    -- max recursion depth
    -> Either p1 p2           -- current player
    -> board
    -> Rose board
gameTree spec@{ availableMoves, switchPlayer } maxDepth player board
    | maxDepth <= 0 = Rose.leaf board
    | otherwise = board :>
        map (gameTree spec (maxDepth - 1) (switchPlayer player)) (availableMoves player board)


minimax
    :: forall board p1 p2
     . GameSpec board p1 p2   -- description of the game
    -> Int                    -- max recursion depth
    -> Either p1 p2           -- current player
    -> Rose board
    -> Rose Int
minimax spec = minimax' spec 0


minimax'
    :: forall board p1 p2
     . GameSpec board p1 p2   -- description of the game
    -> Int                    -- current recursion depth
    -> Int                    -- max recursion depth
    -> Either p1 p2           -- current player
    -> Rose board
    -> Rose Int
minimax' spec@{ switchPlayer, result } depth maxDepth player (board :> boards)
    | depth >= maxDepth = finalScore depth maxDepth (result board)
    | otherwise =
    case NonEmptyArray.fromArray boards of
         -- No more moves left
         Nothing -> finalScore depth maxDepth (result board)

         -- Pick a move
         Just boards' ->
             case player of
                  -- Player 1 is trying to maximise
                  Left _ ->
                    let score = minimax' spec (depth - 1) maxDepth (switchPlayer player)
                        scores = map score boards'
                    in maximum1 (map Rose.root scores) :> NonEmptyArray.toArray scores

                  -- Player 2 is trying to maximise
                  Right _ ->
                    let score = minimax' spec (depth - 1) maxDepth (switchPlayer player)
                        scores = map score boards'
                    in minimum1 (map Rose.root scores) :> NonEmptyArray.toArray scores


-- | Scoring logic (with simple weighting).
finalScore :: forall p1 p2. Int -> Int -> Maybe (Result p1 p2) -> Rose Int
finalScore depth maxDepth = case _ of
    Just (Winner (Left _))  -> Rose.leaf (1    * weight)
    Just (Winner (Right _)) -> Rose.leaf ((-1) * weight)
    Just Draw               -> Rose.leaf 0
    Nothing                 -> Rose.leaf 0
    where weight = maxDepth - depth -- TODO: how to improve this?


-- | Maximum of a non empty Foldable.
maximum1 :: forall a f. Ord a => Foldable1 f => f a -> a
maximum1 = unwrap <<< foldMap1 Max


-- | Maximum of a non empty Foldable.
minimum1 :: forall a f. Ord a => Foldable1 f => f a -> a
minimum1 = unwrap <<< foldMap1 Min


-- | Get the largest elements based on some comparison function.
maximumsBy :: forall a f. Foldable f => (a -> a -> Ordering) -> f a -> Array a
maximumsBy f = foldl go []
  where go :: Array a -> a -> Array a
        go accum a = case Array.uncons accum of
            Nothing -> Array.singleton a
            Just { head } -> case f head a of
                                  LT -> Array.singleton a
                                  EQ -> accum `Array.snoc` a
                                  GT -> accum
