module Games.TicTacToe where

import Prelude
import Data.Maybe (Maybe(Just, Nothing))
import Data.TypeLevel.Nat (Three)
import Data.Foldable (oneOfMap)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Vec (Vec)
import Data.Vec as Vec


data Player = P1 | P2

derive instance eqPlayer :: Eq Player

instance showPlayer :: Show Player where
    show P1 = "X"
    show P2 = "O"


otherPlayer :: Player -> Player
otherPlayer P1 = P2
otherPlayer P2 = P1


newtype Game = Game (Vec Three (Vec Three (Maybe Player)))

derive instance newtypeGame :: Newtype Game _


newGame :: Game
newGame = wrap (Vec.fill (Vec.fill Nothing))


winner :: Game -> Maybe Player
winner (Game game) = rowWinner
    --rowWinner <|> columnWinner <|> diagWinner
  where
    rowWinner :: Maybe Player
    rowWinner = oneOfMap findWinner rows
      where rows = game

    --colWinner :: Maybe Player
    --colWinner = oneOfMap findWinner columns
    --  where columns = map (\j -> Array.mapMaybe (flip Array.index j) game) (0..2) -- FIXME

    --diagWinner :: Maybe Player
    --diagWinner = Nothing  -- TODO


findWinner :: Vec Three (Maybe Player) -> Maybe Player
findWinner = Vec.foldl1 (\p next -> if next == p then p else Nothing)


--instance showGame :: Show Game where
--    show (Game game) =
--        surround ("\n" <> hr <> "\n") $
--            map (surroundMap "|" (maybe " " show)) game
--      where
--        hr :: String
--        hr = fold (Array.replicate (ncol * 2 + 1) "-")
--
--        ncol :: Int
--        ncol = 3  -- FIXME
--
--
---- | Creates a new game with the given dimensions.
--newGame :: { rows :: Int, columns :: Int } -> Game
--newGame { rows, columns } =
--    Game (Array.replicate rows (Array.replicate columns Nothing))
--
--
--exampleGame :: Game
--exampleGame = Game
--    [ [ Just P1, Nothing, Just P2 ]
--    , [ Nothing, Just P2, Nothing ]
--    , [ Just P1, Nothing, Nothing ]
--    ]
--
--
---- TODO
--winner :: Game -> Maybe Player
--winner (Game game) =
--    rowWinner <|> columnWinner <|> diagWinner
--  where
--    rowWinner :: Maybe Player
--    rowWinner = oneOfMap findWinner rows
--      where rows = game
--
--    columnWinner :: Maybe Player
--    columnWinner = oneOfMap findWinner columns
--      where columns = map (\j -> Array.mapMaybe (flip Array.index j) game) (0..2) -- FIXME
--
--    diagWinner :: Maybe Player
--    diagWinner = Nothing  -- TODO
--
--
--findWinner :: Array (Maybe Player) -> Maybe Player
--findWinner = Array.uncons >=> case _ of
--    { head: Nothing, tail } -> Nothing
--    { head, tail } -> foldl (\p next -> if next == p then p else Nothing) head tail
--
--
--nextMove :: Int -> Player -> Game -> Maybe Game
--nextMove maxDepth player game =
--    pickNext (minimax maxDepth player tree)
--  where
--    tree :: Rose Game
--    tree = gameTree maxDepth player game
--
--    pickNext :: Rose Int -> Maybe Game
--    pickNext scores =
--        Tuple.snd <$> maximumBy (comparing Tuple.fst) (Array.zip (root <$> firstBranch scores) (root <$> firstBranch tree))
--
--
--gameTree :: Int -> Player -> Game -> Rose Game
--gameTree maxDepth player game
--    | maxDepth <= 0 = game :> []
--    | otherwise =
--        game :> map (gameTree (maxDepth - 1) (otherPlayer player)) (availableMoves player game)
--
--
--availableMoves :: Player -> Game -> Array Game
--availableMoves player (Game game) =
--    Array.concat $
--        Array.mapWithIndex (\i row ->
--            Array.catMaybes $ Array.mapWithIndex (move i) row
--        ) game
--  where
--    move :: Int -> Int -> Maybe Player -> Maybe Game
--    move i j (Just _) = Nothing
--    move i j Nothing  =
--        Game <$> Array.alterAt i (Array.modifyAt j insertPlayer) game
--
--    insertPlayer :: Maybe Player -> Maybe Player
--    insertPlayer = const (Just player)
--
--
--minimax :: Int -> Player -> Rose Game -> Rose Int
--minimax maxDepth player (game :> games)
--    | maxDepth <= 0 = score game
--    | otherwise =
--    case NonEmptyArray.fromArray games of
--         Nothing -> score game -- no more moves left
--         Just games' ->
--             case player of
--                  -- maximising P1's score
--                  P1 -> let scores = map (minimax (maxDepth - 1) P2) games'
--                         in maximum1 (map root scores) :> NonEmptyArray.toArray scores
--                  -- minimising P2's score
--                  P2 -> let scores = map (minimax (maxDepth - 1) P2) games'
--                         in minimum1 (map root scores) :> NonEmptyArray.toArray scores
--
--score :: Game -> Rose Int
--score game = case winner game of
--                  Just P1 -> 1    :> []
--                  Just P2 -> (-1) :> []
--                  Nothing -> 0    :> []
--
--
---- | Maximum of a non empty Foldable.
--maximum1 :: forall a f. Ord a => Foldable1 f => f a -> a
--maximum1 = unwrap <<< foldMap1 Max
--
--
---- | Maximum of a non empty Foldable.
--minimum1 :: forall a f. Ord a => Foldable1 f => f a -> a
--minimum1 = unwrap <<< foldMap1 Min
