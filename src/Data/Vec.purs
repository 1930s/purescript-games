-- | Length indexed vectors.
-- | Example usage:
-- |
-- | ```purescript
-- | vector :: Vec Nat.Three Int
-- | vector = 0 : 1 : 2 : empty
-- |
-- | second :: Int
-- | second = vector !! Nat.two
-- | ```
module Data.Vec
    ( Vec
    , empty
    , cons
    , (:)
    , index
    , (!!)
    )
where

import Data.Array as Array
import Data.TypeLevel.Nat (kind Nat, S, Z, class Nat, class LessThan, NProxy)
import Data.TypeLevel.Nat as Nat
import Data.TypeLevel.Bool (True)
import Partial.Unsafe (unsafePartial)


-- | Length indexed vectors.
newtype Vec (n :: Nat) (a :: Type) = Vec (Array a)


-- | An empty vector.
empty :: forall a. Vec Z a
empty = Vec []


-- | Cons an element to a vector.
cons :: forall a n. a -> Vec n a -> Vec (S n) a
cons a (Vec as) = Vec (Array.cons a as)
infixr 6 cons as :


-- | Get the value at index `i`.
index :: forall n i a. Nat i => LessThan i n True => Vec n a -> NProxy i -> a
index (Vec array) i = unsafePartial (Array.unsafeIndex array (Nat.toInt i))
--                    ^^^^^^^^^^^^^ Checking is done at the type level :)
infixl 8 index as !!

