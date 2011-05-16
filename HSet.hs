{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, ScopedTypeVariables,
             FlexibleInstances, TypeFamilies, UndecidableInstances, OverlappingInstances #-}
module HSet where

---------- HSet Basics --------------
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

infixr .>.
(.>.) :: (HSet s, HNotMember e s) => e -> s -> e :>: s
(.>.)  = HAdd

type Singleton a = a :>: HTip
singleton :: e -> Singleton e
singleton = (.>. HTip)
hEmpty = HTip

-- test = (42 :: Int) .>. "hi" .>. ('a',12::Int) .>. HTip

---------- HSet API ------------------
class Error x
data TypeNotFound e
data TypeFound e

-- | In order to handle the fact that HSet's are actually type-indexed
-- HLists (they have an ordering), we use hProject to properly reorder
-- arguments to a function
apphfn :: forall s' b s. (HSet s, HSubtype s s') => (s' -> b) -> s -> b
apphfn f = f . hProject

-- | Subtyping: s is a subtype of s' if s includes every type in s'
-- Value-level function hProject will project values out of s in the
-- same order demanded by the type of s'. This is necessary to apply
-- functions that take HSets as arguments
class (HSet s, HSet s') => HSubtype s s' where hProject :: s -> s'
instance (HSet s) => HSubtype s HTip where hProject _ = HTip
instance (HMember e s, HSet s', HSubtype s s') => HSubtype s (HAdd e s') where
    hProject s = HAdd (hMember s) (hProject s)

-- | Membership assertion. Fails at compile time if e is not present in s
class (HSet s) => HMember e s where hMember :: s -> e
instance Error (TypeNotFound e) => HMember e HTip where hMember _ = undefined
instance (HSet s, HNotMember e s) => HMember e (HAdd e s) where hMember (HAdd e _) = e
instance (HSet s, HMember e s) => HMember e (HAdd e' s) where hMember (HAdd _ s) = hMember s

-- | Asserts e is not a member of s. Solely a type-level constraint.
class (HSet s) => HNotMember e s
instance HNotMember e HTip
instance (HNotMember e s) => HNotMember e (HAdd e' s)
instance (Error (TypeFound e), HSet s) => HNotMember e (HAdd e s)

-- | Type level difference, all values in s whose types are present in s' are removed
class HDiff s s' s'' | s s' -> s'' where hDiff :: s -> s' -> s''
instance HDiff s HTip s where hDiff = const
instance (HDelete e s s'', HDiff s'' s' s''') => HDiff s (HAdd e s') s''' where hDiff s (HAdd e s') = hDiff (hDelete e s) s'

-- | Type level union, giving preference to values present in s.
infixl .+.
(.+.) :: (HUnion s s' s'') => s -> s' -> s''
(.+.) = hUnion

class HUnion s s' s'' | s s' -> s'' where hUnion :: s -> s' -> s''
instance HUnion s HTip s where hUnion = const
instance (HElem e s b, HUnionCase b s e s' s'') => HUnion s (HAdd e s') s'' where hUnion s (HAdd e s') = hUnionCase (fst $ hElem e s) s e s'

class (HBool b) => HUnionCase b s e s' s'' | b s e s' -> s'' where hUnionCase :: b -> s -> e -> s' -> s''
instance (HUnion (HAdd e s) s' s'') => HUnionCase HFalse s e s' s'' where hUnionCase _ s e s' = hUnion (HAdd e s) s'
instance (HUnion s s' s'') => HUnionCase HTrue s e s' s'' where hUnionCase _ s _ s' = hUnion s s'

-- | Type level intersection, keeping values present in s.
infixl .*.
(.*.) :: (HIntersection s s' s'') => s -> s' -> s''
(.*.) = hIntersection

class HIntersection s s' s'' | s s' -> s'' where
    hIntersection :: s -> s' -> s''
instance (HIntersection' HTip s s' s'') => HIntersection s s' s'' where
    hIntersection = hIntersection' HTip

class HIntersection' a s s' s'' | a s s' -> s'' where hIntersection' :: a -> s -> s' -> s''
instance HIntersection' a HTip s' a where hIntersection' acc _ _ = acc
instance (HElem e s' b, HIntersectionCase b a e s s' s'') => HIntersection' a (HAdd e s) s' s''
    where hIntersection' a (HAdd e s) s' = hIntersectionCase (fst $ hElem e s') a e s s'

class (HBool b) => HIntersectionCase b a e s s' s'' | b a e s s' -> s'' where hIntersectionCase :: b -> a -> e -> s -> s' -> s''
instance (HIntersection' a s s' s'') => HIntersectionCase HFalse a e s s' s'' where hIntersectionCase _ a _ s s' = hIntersection' a s s'
instance (HIntersection' (HAdd e a) s s' s'') => HIntersectionCase HTrue a e s s' s''
    where hIntersectionCase _ a e s s' = hIntersection' (HAdd e a) s s'

-- | Type level deletion, deletes any value of type e, which need be a witness only
class (HSet s, HSet s') => HDelete e s s' | e s -> s' where hDelete :: e -> s -> s'
instance HDelete e HTip HTip where hDelete _ = id -- empty set
instance (HSet s) => HDelete e (HAdd e s) s where hDelete _ (HAdd _ s) = s -- element found
instance (HSet s, HSet s', HDelete e s s', s'' ~ (HAdd e' s')) => HDelete e (HAdd e' s) s'' where -- recursive case
    hDelete e (HAdd e' s) = HAdd e' (hDelete e s)

-- | Type level merge, will fail if s and s' share a type
class HMerge s s' s'' | s s' -> s'' where hMerge :: s -> s' -> s''
instance HSet s' => HMerge HTip s' s' where hMerge _ = id
instance (HSet s, HSet s', HNotMember e s', HMerge s s' s'') => HMerge (HAdd e s) s' (HAdd e s'') where hMerge (HAdd e s) = HAdd e . (hMerge s)

-- | Type level foldr, first argument (the function, named by a data type) must have an HFoldOp instance
class (HSet s) => HFoldr f v s s' | f v s -> s' where hFoldr :: f -> v -> s -> s'
instance HFoldr f v HTip v where hFoldr _ v _ = v
instance (HSet s, HFoldr f v s s', HFoldOp f e s' s'') => HFoldr f v (HAdd e s) s'' where hFoldr f v (HAdd e s) = hFoldOp f e (hFoldr f v s)

-- | Each function we wish to use with HFoldr needs an HFoldOp instance
class HFoldOp f e s s' | f e s -> s' where hFoldOp :: f -> e -> s -> s'

{-
-- | Experimental HMap in terms of HFoldr
class HMap a b s s' | a b s -> s' where
    hMap :: (a -> b) -> s -> s'
instance (HFoldr (HMapOp a b) HTip s s') => HMap a b s s' where
    hMap f = hFoldr (HMapOp f) HTip

data HMapOp e e' = HMapOp (e -> e')
instance HFoldOp (HMapOp e e') e s (HAdd e' s) where hFoldOp (HMapOp f) e s = HAdd (f e) s
-}

-- | Type level filter, first argument (function) must have an HFilterOp instance
class (HSet s) => HFilter f s s' | f s -> s' where hFilter :: f -> s -> s'
instance HFilter f HTip HTip where hFilter _ _ = HTip
instance (HSet s, HBool b, HFilterOp f e b, HFilterCase b e f s s') => HFilter f (HAdd e s) s' where hFilter f (HAdd e s) = hFilterCase (hFilterOp f e) e f s

class HFilterCase b e f s s' | b e f s -> s' where hFilterCase :: b -> e -> f -> s -> s'
instance (HFilter f s s') => HFilterCase HTrue e f s (HAdd e s') where hFilterCase _ e f s = HAdd e (hFilter f s)
instance (HFilter f s s') => HFilterCase HFalse e f s s' where hFilterCase _ _ f s = hFilter f s

class (HBool b) => HFilterOp f e b | f e -> b where hFilterOp :: f -> e -> b

-- 1. a value of type 'a' must be in s
-- 2. a value of type 'b' must not be in s
-- 3. calculate the type of the set with the 'a' value deleted
-- 4. assert that a value of type 'b' is not already in that (implied by 2 I know, but required by ghc anyway)
hApply :: (HMember a s, HNotMember b s, HDelete a s s', HNotMember b s')
     => (a -> b) -> s -> (HAdd b s')
hApply f s = (f a) .>. s'
    where a = hMember s
          s' = hDelete a s

-- for a container of HSets
hFmap f = fmap (hApply f)

-- | Reverse a HSet (ignore the fact that sets should be unordered!)
-- Depends on an implementation detail of hUnion, probably shouldn't exist
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

class (HSet s, HBool b) => HElem e s b | e s -> b where hElem :: e -> s -> (b,e)
instance HElem e HTip HFalse where hElem _ _ = (HFalse, undefined)
instance (HSet s, HNotMember e s) => HElem e (HAdd e s) HTrue where hElem _ (HAdd e _) = (HTrue,e)
instance (HSet s, HElem e s b) => HElem e (HAdd e' s) b where hElem e (HAdd _ s) = hElem e s

-- | Type AND value level set equality.
-- Sets must be same length, have same types, and values of each type must be equal.
-- As this is set equality, ordering obviously doesn't matter.
infix .==.
(.==.) :: (HEqual s s') => s -> s' -> Bool
(.==.) = hEqual

-- Can probably be replaced with some clever usage of HUnion, HIntersection, or HSubtype
class (HSet s, HSet s') => HEqual s s' where hEqual :: s -> s' -> Bool
instance HEqual HTip HTip where hEqual _ _ = True
instance (HSet s) => HEqual HTip (HAdd e s) where hEqual _ _ = False
instance (HSet s, HElem e s' b, HEqualCase b e s s') => HEqual (HAdd e s) s' where hEqual (HAdd e s) s' = hEqualCase e (hElem e s') s s'

class HEqualCase b e s s' where hEqualCase :: e -> (b,e) -> s -> s' -> Bool
instance HEqualCase HFalse e s s' where hEqualCase _ _ _ _ = False
instance (Eq e, HDelete e s' s'', HEqual s s'') => HEqualCase HTrue e s s' where hEqualCase e (_,e') s s' = (e == e') && (hEqual s (hDelete e' s'))



