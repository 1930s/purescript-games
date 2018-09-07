-- | Rose tree.
module Data.Rose
    ( Rose(Cons)
    , (:>)
    , length
    , root
    , firstBranch
    , leaves
    )
where

import Prelude
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum, intercalate)
import Data.Array as Array


-- | Rose tree.
data Rose a = Cons a (Array (Rose a))

infixr 6 Cons as :>

-- TODO
instance showRose :: Show a => Show (Rose a) where
    show (a :> as) = show a <> intercalate "\n" (map show as)

instance functorRose :: Functor Rose where
    map f (a :> as) = f a :> map (map f) as

instance foldableRose :: Foldable Rose where
    foldMap f (a :> as) = f a <> foldMap (foldMap f) as
    foldl f accum rose = foldl f accum (flatten rose)
    foldr f accum rose = foldr f accum (flatten rose)


-- | Flatten out a rose tree.
flatten :: forall a. Rose a -> Array a
flatten (a :> as) = Array.cons a (foldMap flatten as)


-- | Total length of a rose tree.
length :: forall a. Rose a -> Int
length (a :> []) = 1
length (a :> as) = 1 + sum (map length as)


-- | Root node of a rose tree.
root :: forall a. Rose a -> a
root (a :> _) = a


-- | First branch of a rose tree.
firstBranch :: forall a. Rose a -> Array (Rose a)
firstBranch (_ :> as) = as


-- | Collect up all the leaves of a rose tree.
leaves :: forall a. Rose a -> Array a
leaves (a :> []) = [a]
leaves (a :> as) = Array.cons a (foldMap leaves as)
