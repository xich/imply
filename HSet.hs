{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, TypeFamilies, UndecidableInstances, OverlappingInstances #-}
module HSet where

class Error x
data TypeNotFound e
data TypeFound e

class (HSet s) => HMember e s where hMember :: s -> e
instance Error (TypeNotFound e) => HMember e HTip where hMember _ = undefined
instance (HSet s, HNotMember e s) => HMember e (HAdd e s) where hMember (HAdd e _) = e
instance (HSet s, HMember e s) => HMember e (HAdd e' s) where hMember (HAdd _ s) = hMember s

class (HSet s) => HNotMember e s
instance HNotMember e HTip
instance (HNotMember e s) => HNotMember e (HAdd e' s)
instance (Error (TypeFound e), HSet s) => HNotMember e (HAdd e s)

class (HSet s, HSet s') => HDelete e s s' | e s -> s' where hDelete :: e -> s -> s'
instance HDelete e HTip HTip where hDelete _ = id -- empty set
instance (HSet s) => HDelete e (HAdd e s) s where hDelete _ (HAdd _ s) = s -- element found
instance (HSet s, HSet s', HDelete e s s', s'' ~ (HAdd e' s')) => HDelete e (HAdd e' s) s'' where -- recursive case
    hDelete e (HAdd e' s) = HAdd e' (hDelete e s)

class HMerge s s' s'' | s s' -> s'' where hMerge :: s -> s' -> s''
instance HSet s' => HMerge HTip s' s' where hMerge _ = id
instance (HSet s, HSet s', HNotMember e s', HMerge s s' s'') => HMerge (HAdd e s) s' (HAdd e s'') where hMerge (HAdd e s) = HAdd e . (hMerge s)

-- 1. a value of type 'a' must be in s
-- 2. a value of type 'b' must not be in s
-- 3. calculate the type of the set with the 'a' value deleted
-- 4. assert that a value of type 'b' is not already in that (implied by 2 I know, but required by ghc anyway)
happly :: (HMember a s, HNotMember b s, HDelete a s s', HNotMember b s')
     => (a -> b) -> s -> (HAdd b s')
happly f s = (f a) .>. s'
    where a = hMember s
          s' = hDelete a s
{-
-- note, this can violate the TIP property right now
class HMap f h h' | f h -> h' where hmap :: f -> h -> h'
instance HMap (a -> b) HTip HTip where hmap _ = id -- empty set
instance (HSet s) => HMap (a -> b) (HAdd a s) (HAdd b s) where hmap f (HAdd a s) = HAdd (f a) s -- element found
instance (HSet s, HSet s', s'' ~ (HAdd c s'), HMap (a -> b) s s') => HMap (a -> b) (HAdd c s) s'' where hmap f (HAdd c s) = HAdd c (hmap f s) -- recursive case
-}

data HTrue = HTrue   deriving (Eq,Show,Read)
data HFalse = HFalse deriving (Eq,Show,Read)

class HBool b
instance HBool HTrue
instance HBool HFalse

class HNull s b | s -> b where hNull :: s -> b
instance HNull HTip HTrue where hNull _ = HTrue
instance HNull (HAdd e s) HFalse where hNull _ = HFalse

---------- HSET ----------------------
data HTip = HTip         deriving (Eq)
data HAdd e s = HAdd e s deriving (Eq)

type e :>: s = HAdd e s

class HSet s

instance HSet HTip
instance HSet s => HSet (HAdd e s)

class HSet s => HShow s where hshow :: s -> String
instance HShow HTip where hshow _ = "}"
instance (Show e, HShow s) => HShow (HAdd e s) where hshow (HAdd e s) = "," ++ show e ++ hshow s

instance Show HTip where
    show _ = "{}"

instance (Show a, HShow s) => Show (HAdd a s) where
    show (HAdd e s) = "{" ++ show e ++ hshow s
---------- HSET ----------------------

infixr .>.
(.>.) :: (HSet s, HNotMember e s) => e -> s -> HAdd e s
(.>.)  = HAdd

type Singleton a = a :>: HTip
singleton = (.>. HTip)

hfmap f = fmap (happly f)

test = (42 :: Int) .>. "hi" .>. ('a',12::Int) .>. HTip

