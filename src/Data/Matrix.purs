module Data.Matrix
    ( Matrix
    , rows
    , cols
    , getRow
    , getCol
    )
where

import Prelude
import Data.Index (Index)
import Data.TypeLevel.Nat (class Nat, kind Nat)
import Data.Vec (Vec)
import Data.Vec as Vec


type Matrix (n :: Nat) (m :: Nat) a = Vec n (Vec m a)


-- | Returns the rows of a `Matrix`.
rows :: forall n m a. Matrix n m a -> Vec n (Vec m a)
rows = identity


-- | Returns the columns of a `Matrix`.
cols :: forall n m a. Nat n => Nat m => Matrix n m a -> Vec m (Vec n a)
cols = Vec.transpose


-- | Get the row at a given index.
getRow :: forall n m a. Matrix n m a -> Index n -> Vec m a
getRow = Vec.index


-- | Get the column at a given index.
getCol :: forall n m a. Nat n => Nat m => Matrix n m a -> Index m -> Vec n a
getCol = cols >>> Vec.index
