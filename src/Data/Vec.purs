-- | Length indexed vectors.
module Data.Vec
    ( Vec
    , cons
    , (+>)
    , empty
    , singleton
    , fill
    , fillWith
    , append
    , index
    , (!!)
    , indices
    , updateAt
    , transpose
    , diag
    , head
    , uncons
    , zipWith
    , tail
    , foldl1
    )
where

import Prelude
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Index (Index)
import Data.Index as Index
import Data.Maybe (fromJust)
import Data.TypeLevel.Nat (class Add, class Nat, NProxy(NProxy), S, Z, kind Nat)
import Data.TypeLevel.Nat as Nat
import Partial.Unsafe (unsafePartial)


-- | Length indexed vectors.
newtype Vec (n :: Nat) (a :: Type) = Vec (Array a)

derive newtype instance showVec :: Show a => Show (Vec n a)
derive newtype instance functorVec :: Functor (Vec n)
derive newtype instance foldableVec :: Foldable (Vec n)

instance functorWithIndexVec :: Nat n => FunctorWithIndex (Index n) (Vec n) where
    mapWithIndex f = zipWith f indices


-- | Cons an element to a vector.
cons :: forall a n. a -> Vec n a -> Vec (S n) a
cons a (Vec as) = Vec (Array.cons a as)
infixr 6 cons as +>


-- | An empty vector.
empty :: forall a. Vec Z a
empty = Vec []


-- | A singleton vector.
singleton :: forall a. a -> Vec (S Z) a
singleton a = Vec [a]


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


-- | Safely index into a vector.
index :: forall n a. Vec n a -> Index n -> a
index (Vec as) i = unsafePartial (Array.unsafeIndex as (Index.toInt i))
infixl 8 index as !!


-- | Valid indices for a given length vector.
indices :: forall n. Nat n => Vec n (Index n)
indices = Vec (map toIndex (Array.range 0 stop))
  where
    len :: NProxy n
    len = NProxy

    stop :: Int
    stop = Nat.toInt len - 1

    toIndex :: Int -> Index n
    toIndex i = unsafePartial (fromJust (Index.fromInt i len))


-- | Change the element at the given index.
updateAt :: forall n a. Index n -> a -> Vec (S n) a -> Vec (S n) a
updateAt i a (Vec as) =
    Vec (unsafePartial (fromJust (Array.updateAt (Index.toInt i) a as)))


-- | Head of a non-zero-length vector.
head :: forall a n. Vec (S n) a -> a
head (Vec as) = unsafePartial (fromJust (Array.head as))


-- | Tail of a non-zero-length vector.
tail :: forall a n. Vec (S n) a -> Vec n a
tail (Vec as) = Vec (unsafePartial (fromJust (Array.tail as)))


-- | Uncons a non-zero-length vector.
uncons :: forall a n. Vec (S n) a -> { head :: a, tail :: Vec n a }
uncons vec = { head: head vec, tail: tail vec }


-- | Zippy.
zipWith :: forall a b c n. (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (Vec as) (Vec as') = Vec (Array.zipWith f as as')


-- | Fold a non-zero-length vector.
foldl1 :: forall a n. (a -> a -> a) -> Vec (S n) a -> a
foldl1 f vec = foldl f (head vec) (tail vec)


-- | Transpose.
transpose :: forall n m a. Nat n => Nat m => Vec n (Vec m a) -> Vec m (Vec n a)
transpose rows = map (\j -> map (_ !! j) rows) indices


-- | Get the diagonal of a square matrix.
diag :: forall n a. Nat n => Vec n (Vec n a) -> Vec n a
diag = mapWithIndex (flip index)
