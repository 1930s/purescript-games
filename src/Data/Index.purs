module Data.Index
    ( Index
    , fromInt
    , toInt
    )
where

import Prelude
import Data.Maybe (Maybe(Just, Nothing))
import Data.TypeLevel.Nat (kind Nat, class Nat, NProxy(NProxy))
import Data.TypeLevel.Nat as Nat


-- | An index is an natural strictly less than some bound.
newtype Index (n :: Nat) = Index Int

derive newtype instance showIndex :: Show (Index n)


-- | Create an index from an integer.
fromInt :: forall n. Nat n => Int -> Maybe (Index n)
fromInt i
    | i < 0 = Nothing
    | i >= Nat.toInt (NProxy :: NProxy n) = Nothing
    | otherwise = Just (Index i)


-- | Get back an integer.
toInt :: forall n. Index n -> Int
toInt (Index i) = i
