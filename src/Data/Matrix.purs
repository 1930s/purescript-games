module Data.Matrix
    ( Matrix
    , rows
    , cols
    )
where

import Prelude
import Data.Array as Array
import Data.TypeLevel.Nat (kind Nat, NProxy(NProxy))
import Data.TypeLevel.Nat as Nat
import Data.Vec (Vec)
import Data.Vec as Vec


newtype Matrix (n :: Nat) (m :: Nat) a
    = Matrix (Vec n (Vec m a))


rows :: forall n m a. Matrix n m a -> Vec n (Vec m a)
rows (Matrix rs) = rs


cols :: forall n m a. Matrix n m a -> Vec m (Vec n a)
cols (Matrix rs) = map ?hole rs -- (Array.range 0 ncol)
    --where ncol = Nat.toInt (NProxy :: NProxy m)


