{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables,
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

-- | Type level difference, all values in first set whose types are present in second set are removed
class HDiff s s' s'' | s s' -> s'' where hDiff :: s -> s' -> s''
instance HDiff s HTip s where hDiff = const
instance (HDelete e s s'', HDiff s'' s' s''') => HDiff s (HAdd e s') s''' where hDiff s (HAdd e s') = hDiff (hDelete e s) s'

-- | Type level union, giving preference to values present in the first set.
class HUnion s s' s'' | s s' -> s'' where hUnion :: s -> s' -> s''
instance HUnion s HTip s where hUnion = const
instance (HElem e s b, HUnionCase b s e s' s'') => HUnion s (HAdd e s') s'' where hUnion s (HAdd e s') = hUnionCase (hElem e s) s e s'

class (HBool b) => HUnionCase b s e s' s'' | b s e s' -> s'' where hUnionCase :: b -> s -> e -> s' -> s''
instance (HUnion (HAdd e s) s' s'') => HUnionCase HFalse s e s' s'' where hUnionCase _ s e s' = hUnion (HAdd e s) s'
instance (HUnion s s' s'') => HUnionCase HTrue s e s' s'' where hUnionCase _ s _ s' = hUnion s s'

-- | Type level deletion, deletes any value matching the type of the first argument, which is a witness only
class (HSet s, HSet s') => HDelete e s s' | e s -> s' where hDelete :: e -> s -> s'
instance HDelete e HTip HTip where hDelete _ = id -- empty set
instance (HSet s) => HDelete e (HAdd e s) s where hDelete _ (HAdd _ s) = s -- element found
instance (HSet s, HSet s', HDelete e s s', s'' ~ (HAdd e' s')) => HDelete e (HAdd e' s) s'' where -- recursive case
    hDelete e (HAdd e' s) = HAdd e' (hDelete e s)

-- | Type level merge, will fail if sets share a type
class HMerge s s' s'' | s s' -> s'' where hMerge :: s -> s' -> s''
instance HSet s' => HMerge HTip s' s' where hMerge _ = id
instance (HSet s, HSet s', HNotMember e s', HMerge s s' s'') => HMerge (HAdd e s) s' (HAdd e s'') where hMerge (HAdd e s) = HAdd e . (hMerge s)

-- 1. a value of type 'a' must be in s
-- 2. a value of type 'b' must not be in s
-- 3. calculate the type of the set with the 'a' value deleted
-- 4. assert that a value of type 'b' is not already in that (implied by 2 I know, but required by ghc anyway)
hApply :: (HMember a s, HNotMember b s, HDelete a s s', HNotMember b s')
     => (a -> b) -> s -> (HAdd b s')
hApply f s = (f a) .>. s'
    where a = hMember s
          s' = hDelete a s

-- | Reverse a HSet (ignore the fact that sets should be unordered!)
-- Depends on an implementation detail of hUnion
hReverse :: (HUnion HTip s s') => s -> s'
hReverse s = hUnion hEmpty s

{-
-- note, this can violate the TIP property right now
class HMap f h h' | f h -> h' where hmap :: f -> h -> h'
instance HMap (a -> b) HTip HTip where hmap _ = id -- empty set
instance (HSet s) => HMap (a -> b) (HAdd a s) (HAdd b s) where hmap f (HAdd a s) = HAdd (f a) s -- element found
instance (HSet s, HSet s', s'' ~ (HAdd c s'), HMap (a -> b) s s') => HMap (a -> b) (HAdd c s) s'' where hmap f (HAdd c s) = HAdd c (hmap f s) -- recursive case
-}

-- Heterogeneous Predicates
data HTrue = HTrue   deriving (Eq,Show,Read)
data HFalse = HFalse deriving (Eq,Show,Read)

class HBool b where true :: b -> Bool
instance HBool HTrue where true _ = True
instance HBool HFalse where true _ = False

-- These never fail at compile time
class HNull s b | s -> b where hNull :: s -> b
instance HNull HTip HTrue where hNull _ = HTrue
instance HNull (HAdd e s) HFalse where hNull _ = HFalse

class (HSet s, HBool b) => HElem e s b | e s -> b where hElem :: e -> s -> b
instance HElem e HTip HFalse where hElem _ _ = HFalse
instance (HSet s, HNotMember e s) => HElem e (HAdd e s) HTrue where hElem _ (HAdd _ _) = HTrue
instance (HSet s, HElem e s b) => HElem e (HAdd e' s) b where hElem e (HAdd _ s) = hElem e s

---------- HSET ----------------------
data HTip = HTip         deriving (Eq)
data HAdd e s = HAdd e s deriving (Eq)

type e :>: s = HAdd e s

class HSet s where witness :: s

instance HSet HTip where witness = HTip
instance HSet s => HSet (HAdd e s) where witness = HAdd (error "witness" :: e) (witness :: s)

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
hEmpty = HTip

hFmap f = fmap (hApply f)

test = (42 :: Int) .>. "hi" .>. ('a',12::Int) .>. HTip

