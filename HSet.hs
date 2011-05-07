{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module HSet where

data HTrue = HTrue
data HFalse = HFalse

class HBool b
instance HBool HTrue
instance HBool HFalse

class (HBool b, HSet s) => HMember e s b | e s -> b where
    hMember :: e -> s -> b

instance HMember e HTip HFalse where
    hMember _ _ = HFalse

instance (HSet s, e ~ e') => HMember e (HAdd e' s) HTrue where
    hMember _ _ = HTrue

instance (HSet s, HMember e s b) => HMember e (HAdd e' s) b where
    hMember e (HAdd _ s) = hMember e s

data HTip = HTip         deriving (Eq,Show,Read)
data HAdd e s = HAdd e s deriving (Eq,Show,Read)

type e :*: s = HAdd e s

infixr .*.
(.*.) :: (HSet s) => e -> s -> HAdd e s
(.*.)  = HAdd

class HSet s

instance HSet HTip
instance HSet s => HSet (HAdd e s)

test = (42 :: Int) .*. "hi" .*. ('a',12::Int) .*. HTip
