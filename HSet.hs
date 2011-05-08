{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, TypeFamilies, UndecidableInstances, OverlappingInstances #-}
module HSet where

import Burglar
import Conditional
import Distribution
import Variable

data HTrue = HTrue   deriving (Eq,Show,Read)
data HFalse = HFalse deriving (Eq,Show,Read)

class HBool b
instance HBool HTrue
instance HBool HFalse

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

class HNull s b | s -> b where hNull :: s -> b
instance HNull HTip HTrue where hNull _ = HTrue
instance HNull (HAdd e s) HFalse where hNull _ = HFalse

class HMerge s s' s'' | s s' -> s'' where hMerge :: s -> s' -> s''
instance HSet s' => HMerge HTip s' s' where hMerge _ = id
instance (HSet s, HSet s', HNotMember e s', HMerge s s' s'') => HMerge (HAdd e s) s' (HAdd e s'') where hMerge (HAdd e s) = HAdd e . (hMerge s)

class TypeCast x y | x -> y, y -> x where typeCast :: x -> y

class Error x
data TypeNotFound e
data TypeFound e

data HTip = HTip         deriving (Eq,Show,Read)
data HAdd e s = HAdd e s deriving (Eq,Show,Read)

type e :>: s = HAdd e s

infixr .>.
(.>.) :: (HSet s, HNotMember e s) => e -> s -> HAdd e s
(.>.)  = HAdd

singleton = (.>. HTip)

class HSet s

instance HSet HTip
instance HSet s => HSet (HAdd e s)

test = (42 :: Int) .>. "hi" .>. ('a',12::Int) .>. HTip

-- Distributions with heterogeneous sets
newtype HP ha = HP { unHP :: [(ha,Float)] }
    deriving (Show)

instance Functor HP where fmap f = HP . map (\(ha,p) -> (f ha,p)) . unHP

pToHP :: P a -> HP (HAdd a HTip)
pToHP = mkHP . (flip concatMap domain) . unC

mkHP :: [(a,Float)] -> HP (HAdd a HTip)
mkHP = HP . map (\(a,p) -> (singleton a, p))

{-
class HFunctor f where
    hfmap :: (HMember a s, HNotMember b s, HDelete a s s', HNotMember b s') => (a -> b) -> f s -> f (HAdd b s')

instance HFunctor HP where
    hfmap f = HP . map (\(ha,p) -> (happly f ha,p)) . unHP
-}
hfmap f = fmap (happly f)

newtype HC ha hb = HC { unHC :: ha -> [(hb,Float)] }

mkHC :: (a -> [(b,Float)]) -> HC (HAdd a HTip) (HAdd b HTip)
mkHC f = undefined

{-
class HUpdate e e' s s' | e e' s -> s' where hUpdate :: e -> e' -> s -> s'
-- empty case
instance HUpdate e e' HTip HTip where hUpdate _ _ = id
-- instance (HNotMember e s, HSet s) => HUpdate e e' s s where hUpdate _ _ = id
-- update case
instance (HNotMember e s, HSet s) => HUpdate e e' (HAdd e s) (HAdd e' s) where hUpdate _ e' (HAdd _ s) = HAdd e' s
-- not found, recurse case
instance (HNotMember e (e'' :>: HTip), HUpdate e e' s s') => HUpdate e e' (HAdd e'' s) (HAdd e'' s') where hUpdate e e' (HAdd e'' s) = HAdd e'' (hUpdate e e' s)
-- instance HUpdate e e' (HAdd e s) (HAdd e' s) where hUpdate _ e' (HAdd _ s) = HAdd e' s
-- instance (HUpdate e e' s s') => HUpdate e e' (HAdd e'' s) (HAdd e'' s') where hUpdate e e' (HAdd e'' s) = HAdd e'' (hUpdate e e' s)

instance (HList l, HDeleteMany e l l', TypeCast (HCons e' l') l'') => HDeleteMany e (HCons e' l) l'' where
    hDeleteMany p (HCons e' l) = typeCast (HCons e' (hDeleteMany p l))
-}

