{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module ExtendedData where
import CommonData
import System.IO

class Valuable a where
    toValue :: a -> Value
    fromValue :: Value -> a




instance Valuable String where
    toValue = StringV
    fromValue = unString

instance (Valuable a, Valuable b) => Valuable (a -> b) where
    toValue a = Lam (toValue . a . fromValue)
    fromValue a = fromValue . unLam a . toValue

instance Valuable a => Valuable (IO a) where
    toValue a = IO (fmap toValue a)
    fromValue a = fmap fromValue (unIO a)

instance Valuable Int where
    toValue = IntV
    fromValue = unInt
instance Valuable Double where
    toValue = DoubleV
    fromValue = unDouble

{-
instance Valuable a => Valuable [a] where
    toValue [] = UnitV
    toValue (a:rest) = TupleV [toValue a, toValue rest]
    fromValue UnitV = []
    fromValue (TupleV [a, rest]) = fromValue a : fromValue rest
-}


instance Valuable a => Valuable (Maybe a) where
    toValue Nothing = UnitV
    toValue (Just x) = TupleV [toValue x, UnitV]
    fromValue UnitV = Nothing
    fromValue (TupleV [a, _]) = Just (fromValue a)

instance Valuable () where
    toValue () = UnitV
    fromValue UnitV = ()

instance Valuable Handle where
    toValue = Handle
    fromValue = unHandle