module Data.Index
    ( Index
    , fromInt
    , toInt
    )
where

import Prelude
import Data.Maybe (Maybe(Just, Nothing))
import Data.TypeLevel.Nat (kind Nat, class Nat, NProxy)
import Data.TypeLevel.Nat as Nat


-- | An index is an integer strictly less than some bound.
newtype Index (n :: Nat) = Index Int

derive newtype instance showIndex :: Show (Index n)


-- | Construct an index that must be less than some bound.
fromInt :: forall n. Nat n => Int -> NProxy n -> Maybe (Index n)
fromInt i bound
    | i < 0 = Nothing
    | i >= Nat.toInt bound = Nothing
    | otherwise = Just (Index i)


-- | Get back an integer.
toInt :: forall n. Index n -> Int
toInt (Index i) = i
