-- | Type level Natural numbers.
module Data.TypeLevel.Nat
    ( kind Nat
    , Z
    , S
    , NProxy(NProxy)

    -- Converting back to an `Int`
    , class Nat

    -- Arithmetic classes
    , class Add

    -- Predicate classes
    , class NonZero

    , class LessThan
    , toInt

    -- Synonyms
    , Zero,  zero
    , One,   one
    , Two,   two
    , Three, three
    , Four,  four
    , Five,  five
    , Six,   six
    , Seven, seven
    , Eight, eight
    , Nine,  nine
    , Ten,   ten
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
    toInt   :: NProxy n -> Int

instance natZ :: Nat Z where
    toInt _   = 0

instance natS :: Nat n => Nat (S n) where
    toInt _   = 1 + toInt (NProxy :: NProxy n)


-- | Add two type level naturals.
class Add (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c
instance addDone :: Add Z b b
instance addRec  :: Add a b c => Add (S a) b (S c)


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
zero = NProxy :: NProxy Zero

type One = S Z
one = NProxy :: NProxy One

type Two = S (S Z)
two = NProxy :: NProxy Two

type Three = S (S (S Z))
three = NProxy :: NProxy Three

type Four = S (S (S (S Z)))
four = NProxy :: NProxy Four

type Five = S (S (S (S (S Z))))
five = NProxy :: NProxy Five

type Six = S (S (S (S (S (S Z)))))
six = NProxy :: NProxy Six

type Seven = S (S (S (S (S (S Z)))))
seven = NProxy :: NProxy Seven

type Eight = S (S (S (S (S (S Z)))))
eight = NProxy :: NProxy Eight

type Nine = S (S (S (S (S (S (S Z))))))
nine = NProxy :: NProxy Nine

type Ten = S (S (S (S (S (S (S (S Z)))))))
ten = NProxy :: NProxy Ten
