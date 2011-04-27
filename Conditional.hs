module Conditional where

import Distribution
import Util

import Control.Applicative
-- probably would clean up some defs
-- import Control.Arrow
import Data.Function
import qualified Data.List as List

-- Old definition:
--
--      newtype C a b = C { unC :: [(a,b,Float)] }
--
-- Representing with the new definition below
-- allows us to implement pure cleanly. Before,
-- we had to generate an 'a' value using magic
-- and pixie dust. Also allows us to define
-- conditional distributions rather naturally:
--
--      atob :: Int -> [(Char,Float)]
--      atob 1 = [('a',0.6),('b',0.4)]
--      atob 2 = [('a',0.3),('b',0.7)]
--
--      bgivena :: C Int Char
--      bgivena = makeC atob
newtype C a b = C { unC :: a -> [(b,Float)] }
-- type P a = C () a ???

makeC :: (a -> [(b,Float)]) -> C a b
makeC f = C $ normalize . f

fromList :: (Eq a) => [(a,b,Float)] -> C a b
fromList ps = makeC (\a -> [(b,f) | (a',b,f) <- ps, a == a'])

instance Functor (C a) where
    fmap f (C c) = C (\a -> [(f b,p) | (b,p) <- c a])

instance Applicative (C a) where
    pure x          = C (\_ -> [(x,1.0)])
    (C f) <*> (C g) = C (\a -> [ (h b, p*q)
                               | (h,p) <- f a
                               , (b,q) <- g a
                               ])

-- note: P(A,B) = P(B|A)P(A)
prod :: (Eq a) => C a b -> P a -> P (a,b)
prod (C c) (P ps) = P [((a,b),p*q) | (a,p) <- ps, (b,q) <- c a]

-- P(B|A) = P(A,B)/P(A)
-- would be nice to have unordered tuples here
divl :: (Eq a) => P (a,b) -> P a -> C a b
divl (P ts) (P ps) = C (\a -> [ (b,p/q)
                              | ((_,b),p) <- filter ((== a) . fst . fst) ts
                              , (    _,q) <- filter ((== a) . fst) ps])

-- P(B|A) = P(A,B)/P(B)
divr :: (Eq b) => P (a,b) -> P b -> C b a
divr bas = divl (fmap swap bas)

{-
-- P(B|A) - Conditional Probability
newtype C a b = C { unC :: [(a,b,Float)] }
    deriving (Eq)
-- type P a = C () a -- eventually?

instance (Show a, Show b) => Show (C a b) where
    show (C ps) = unlines [show b ++ "|" ++ show a ++ ": " ++ show p | (a,b,p) <- ps]

instance Functor (C a) where
    fmap f (C ps) = C [(a,f b,p) | (a,b,p) <- ps]

instance (Eq a) => Applicative (C a) where
    pure x = C [(error "pure (C a) - this should always be ()",x,1.0)]
    (C fs) <*> (C ps) = C [(a,f b,p*q) | (a,f,p) <- fs, (a',b,q) <- ps, a == a']

instance (Eq a) => Monad (C a) where
    return = pure
    (C ps) >>= k = C [(a,v',p*q) | (a,v,p) <- ps, (a',v',q) <- unC $ k v, a == a' ]

-- note: P(A,B) = P(B|A)P(A)
prod :: (Eq a) => C a b -> P a -> P (a,b)
prod (C cs) (P ps) = P [((a,b),p*q) | (a,p) <- ps, (a',b,q) <- cs, a == a']

-- P(B|A) = P(A,B)/P(A)
-- would be nice to have unordered tuples here
divl :: (Eq a) => P (a,b) -> P a -> C a b
divl (P ts) (P ps) = C [(a,b,p/q) | ((a',b),p) <- ts, (a,q) <- ps, a == a']

-- P(B|A) = P(A,B)/P(B)
divr :: (Eq b) => P (a,b) -> P b -> C b a
divr bas = divl (fmap swap bas)

-- horribly inefficient I know
normc :: Ord a => [(a,b,Float)] -> C a b
normc ps = C [(a,b,f/z) | (ps',z) <- zip grps zs, (a,b,f) <- ps']
    where grps = List.groupBy ((==) `on` fst3) $ List.sortBy (compare `on` fst3) ps
          zs = map (sum . (map thd3)) grps
-}
