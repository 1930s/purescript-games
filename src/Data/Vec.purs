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

    -- Constructing vectors
    , cons
    , (:)
    , empty
    , fill
    , fillWith

    -- Safe stuff
    , append
    , index
    , (!!)
    , head
    , tail
    , foldl1
    )
where

import Prelude
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.TypeLevel.Bool (True)
import Data.Maybe (fromJust)
import Data.TypeLevel.Nat (kind Nat, S, Z, class Nat, class Add, class LessThan, NProxy(NProxy))
import Data.TypeLevel.Nat as Nat
import Partial.Unsafe (unsafePartial)


-- | Length indexed vectors.
newtype Vec (n :: Nat) (a :: Type) = Vec (Array a)

derive newtype instance showVec :: Show a => Show (Vec n a)
derive newtype instance functorVec :: Functor (Vec n)
derive newtype instance foldableVec :: Foldable (Vec n)


-- | Cons an element to a vector.
cons :: forall a n. a -> Vec n a -> Vec (S n) a
cons a (Vec as) = Vec (Array.cons a as)
infixr 6 cons as :


-- | An empty vector.
empty :: forall a. Vec Z a
empty = Vec []


-- | Construct a new vector by repeating a specific valu.e
fill :: forall a n. Nat n => a -> Vec n a
fill a = fillWith (const a)


-- | Construct a new vector with a specific filling function.
fillWith :: forall a n. Nat n => (Int -> a) -> Vec n a
fillWith f = Vec (map f $ Array.range start stop)
  where start = 0
        stop  = Nat.toInt (NProxy :: NProxy n) - 1


-- | Append two vectors.
append :: forall a n1 n2 n3. Add n1 n2 n3 => Vec n1 a -> Vec n2 a -> Vec n3 a
append (Vec lhs) (Vec rhs) = Vec (lhs <> rhs)


-- | Get the value at index `i`.
index :: forall n i a. Nat i => LessThan i n True => Vec n a -> NProxy i -> a
index (Vec as) i = unsafePartial (Array.unsafeIndex as (Nat.toInt i))
infixl 8 index as !!


-- | Head of a non-zero-length vector.
head :: forall a n. Vec (S n) a -> a
head (Vec as) = unsafePartial (fromJust (Array.head as))


-- | Tail of a non-zero-length vector.
tail :: forall a n. Vec (S n) a -> Vec n a
tail (Vec as) = Vec (unsafePartial (fromJust (Array.tail as)))


-- | Fold a non-zero-length vector.
foldl1 :: forall a n. (a -> a -> a) -> Vec (S n) a -> a
foldl1 f vec = foldl f (head vec) (tail vec)


-- NOTE: type case is always done through instances of a class
class HasRange (n :: Nat) where
    range :: NProxy n -> Vec n Int


--instance hasRangeZ :: HasRange Z where
--    range _ = empty
--
--
--instance hasRangeS :: HasRange s where
--    range _ =
--
