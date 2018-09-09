-- | Length indexed vectors.
module Data.Vec
    ( Vec
    , cons
    , (+>)
    , empty
    , toArray
    , singleton
    , fill
    , fillWith
    , append
    , index
    , (!!)
    , range
    , updateAt
    , modifyAt
    , transpose
    , diag
    , diag'
    , head
    , reverse
    , uncons
    , zip
    , zipWith
    , tail
    , foldl1
    )
where

import Prelude
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Index (Index)
import Data.Index as Index
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(Tuple))
import Data.TypeLevel.Nat (class Add, class Nat, NProxy(NProxy), S, Z, kind Nat)
import Data.TypeLevel.Nat as Nat
import Partial.Unsafe (unsafePartial)


-- | Length indexed vectors.
newtype Vec (n :: Nat) (a :: Type) = Vec (Array a)

derive newtype instance showVec :: Show a => Show (Vec n a)
derive newtype instance functorVec :: Functor (Vec n)
derive newtype instance foldableVec :: Foldable (Vec n)

instance functorWithIndexVec :: Nat n => FunctorWithIndex (Index n) (Vec n) where
    mapWithIndex f = zipWith f range

instance foldableWithIndexVec :: Nat n => FoldableWithIndex (Index n) (Vec n) where
    foldrWithIndex f b as =
        foldr (\(Tuple i a) b' -> f i a b') b (zip range as)
    foldlWithIndex f b as =
        foldl (\b' (Tuple i a) -> f i b' a) b (zip range as)
    foldMapWithIndex = foldMapWithIndexDefaultL


cons :: forall a n. a -> Vec n a -> Vec (S n) a
cons a (Vec as) = Vec (Array.cons a as)
infixr 6 cons as +>


empty :: forall a. Vec Z a
empty = Vec []


singleton :: forall a. a -> Vec (S Z) a
singleton a = Vec [a]


toArray :: forall n a. Vec n a -> Array a
toArray (Vec as) = as


-- | Construct a new vector by repeating a specific valu.e
fill :: forall a n. Nat n => a -> Vec n a
fill a = fillWith (const a)


-- | Construct a new vector with a specific filling function.
fillWith :: forall a n. Nat n => (Int -> a) -> Vec n a
fillWith f = Vec (map f $ Array.range start stop)
  where start = 0
        stop  = Nat.toInt (NProxy :: NProxy n) - 1


append :: forall a n1 n2 n3. Add n1 n2 n3 => Vec n1 a -> Vec n2 a -> Vec n3 a
append (Vec lhs) (Vec rhs) = Vec (lhs <> rhs)


-- | Safely index into a vector.
index :: forall n a. Vec n a -> Index n -> a
index (Vec as) i = unsafePartial (Array.unsafeIndex as (Index.toInt i))
infixl 8 index as !!


-- | Valid indices for a given length vector.
range :: forall n. Nat n => Vec n (Index n)
range = Vec (map toIndex (Array.range 0 stop))
  where
    stop :: Int
    stop = Nat.toInt (NProxy :: NProxy n) - 1

    toIndex :: Int -> Index n
    toIndex i = unsafePartial (fromJust (Index.fromInt i))


updateAt :: forall n a. Index n -> a -> Vec n a -> Vec n a
updateAt i a (Vec as) =
    Vec (unsafePartial (fromJust (Array.updateAt (Index.toInt i) a as)))


modifyAt :: forall n a. Index n -> (a -> a) -> Vec n a -> Vec n a
modifyAt i f (Vec as) =
    Vec (unsafePartial (fromJust (Array.modifyAt (Index.toInt i) f as)))


head :: forall a n. Vec (S n) a -> a
head (Vec as) = unsafePartial (fromJust (Array.head as))


tail :: forall a n. Vec (S n) a -> Vec n a
tail (Vec as) = Vec (unsafePartial (fromJust (Array.tail as)))


uncons :: forall a n. Vec (S n) a -> { head :: a, tail :: Vec n a }
uncons vec = { head: head vec, tail: tail vec }


zip :: forall a b n. Vec n a -> Vec n b -> Vec n (Tuple a b)
zip (Vec as) (Vec bs) = Vec (Array.zip as bs)


zipWith :: forall a b c n. (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (Vec as) (Vec bs) = Vec (Array.zipWith f as bs)


foldl1 :: forall a n. (a -> a -> a) -> Vec (S n) a -> a
foldl1 f vec = foldl f (head vec) (tail vec)


transpose :: forall n m a. Nat n => Nat m => Vec n (Vec m a) -> Vec m (Vec n a)
transpose rows = map (\j -> map (_ !! j) rows) range


reverse :: forall n a. Vec n a -> Vec n a
reverse (Vec as) = Vec (Array.reverse as)


diag :: forall n a. Nat n => Vec n (Vec n a) -> Vec n a
diag = mapWithIndex (flip index)


diag' :: forall n a. Nat n => Vec n (Vec n a) -> Vec n a
diag' = mapWithIndex (\i row -> reverse row !! i)
