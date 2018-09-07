-- | Type level Natural numbers.
module Data.TypeLevel.Nat
    ( kind Nat
    , Z
    , S
    , NProxy(NProxy)

    -- Converting back to an `Int`
    , class Nat

    -- Predicate classes
    , class NonZero

    , class LessThan
    , toInt

    -- Synonyms
    , Zero
    , zero
    , One
    , one
    , Two
    , two
    , Three
    , three
    )
where

import Prelude
import Data.TypeLevel.Bool (kind Bool, True, False)


foreign import kind Nat
foreign import data Z :: Nat
foreign import data S :: Nat -> Nat


-- | Value level proxy.
data NProxy (n :: Nat) = NProxy


-- | Converting back to an `Int`.
class Nat (n :: Nat) where
    toInt :: NProxy n -> Int

instance natZ :: Nat Z where
    toInt _ = 0

instance natS :: Nat n => Nat (S n) where
    toInt _ = 1 + toInt (NProxy :: NProxy n)


-- | Test that a `Nat` is greater than zero.
class NonZero (n :: Nat) (b :: Bool) | n -> b
instance nonZeroFalse :: NonZero Z False
instance nonZeroTrue  :: NonZero (S n) True


-- | Test that one `Nat` is less than another.
class LessThan (a :: Nat) (b :: Nat) (c :: Bool) | a b -> c
instance lessThanTrue  :: LessThan Z (S b) True
instance lessThanFalse :: LessThan (S a) Z False
instance lessThanRec   :: LessThan a b c => LessThan (S a) (S b) c


{- Synonyms -}


type Zero = Z

zero :: NProxy Zero
zero = NProxy

type One = S Z

one :: NProxy One
one = NProxy

type Two = S (S Z)

two :: NProxy Two
two = NProxy

type Three = S (S (S Z))

three :: NProxy Three
three = NProxy
