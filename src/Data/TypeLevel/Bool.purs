-- | Type level Booleans.
module Data.TypeLevel.Bool
    ( kind Bool
    , True
    , False
    , BProxy(BProxy)

    -- Converting back to a `Boolean`
    , class Bool
    , toBoolean
    )
where


foreign import kind Bool
foreign import data True  :: Bool
foreign import data False :: Bool


-- | Value level proxy.
data BProxy (b :: Bool) = BProxy


-- | Converting back to a `Boolean`.
class Bool (b :: Bool) where
    toBoolean :: BProxy b -> Boolean

instance boolTrue  :: Bool True  where toBoolean _ = true
instance boolFalse :: Bool False where toBoolean _ = false
