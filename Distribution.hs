-- Compare with PFP: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
module Distribution where

import HSet
import Utils
import Conditional
import Variable

import Control.Applicative
import Data.Function

-- Distributions are just conditional distributions where
-- the conditioning variable only has one possible value
-- which happens 100% of the time: unit.
type HP a = HC (Singleton ()) a

-- Distributions
weighted :: Eq a => [(a,Float)] -> HP (Singleton a)
weighted = mkHC . const

uniform :: Eq a => [a] -> HP (Singleton a)
uniform xs = weighted $ zip xs (repeat 1)

enum :: (Bounded a, Enum a, Eq a) => [Float] -> HP (Singleton a)
enum = weighted . zip [minBound..maxBound]

-- Contrast this with the PFP definition:
--
--      joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
--      joinWith f (D d) (D d') = D [(f x y,p*q) | (x,p) <- d, (y,q) <- d']
--
-- Our applicative functor gives us this for free, and opens the possibility
-- for other representations.
--
--      joinWith :: (a -> b -> c) -> P a -> P b -> P c
--      joinWith f as bs = f <$> as <*> bs
product :: (Applicative p) => p a -> p b -> p (a,b)
product as bs = (,) <$> as <*> bs

-- note: P(A,B) = P(B|A)P(A)
-- this is different in product above, because we are chaining
-- prod :: (Eq a) => C a b -> P a -> P (a,b) -- the following is more general
{-
prod :: (Eq a) => C a b -> C u a -> C u (a,b)
prod (C bga) (C as) = C (\u -> [((a,b),p*q) | (a,p) <- as u, (b,q) <- bga a])

-- P(B|A) = P(A,B)/P(A)
-- would be nice to have unordered tuples here
divl :: (Eq a) => HP (a,b) -> HP a -> HC a b
divl (HC abs) (HC as) = HC (\a -> [ (b,p/q)
                              | ((_,b),p) <- filter ((== a) . fst . fst) $ concatMap abs domain
                              , (    _,q) <- filter ((== a) . fst) $ concatMap as domain])
{-
divl (P ts) (P ps) = C (\a -> [ (b,p/q)
                              | ((_,b),p) <- filter ((== a) . fst . fst) ts
                              , (    _,q) <- filter ((== a) . fst) ps])
-}

-- P(B|A) = P(A,B)/P(B)
divr :: (Eq b) => HP (a,b) -> HP b -> HC b a
divr bas = divl (fmap swap bas)
-}

{-
-- P(A) - Probability Distribution
newtype P a = P { unP :: [(a,Float)] }
    deriving (Eq)

instance Show a => Show (P a) where
    show (P ps) = unlines [show x ++ ": " ++ show p | (x,p) <- ps]

instance Functor P where
    fmap f (P ps) = P [(f a,p) | (a,p) <- ps]

-- Note: We can model independent events (like a dice roll)
-- with an Applicative Functor. Intuitively, this make sense,
-- as the major difference between an Applicative Functor and a
-- Monad is that bind can choose the next computation based on the
-- result of the current computation, where ap doesn't have that power.
instance Applicative P where
    pure x = P [(x,1.0)]
    (P fs) <*> (P xs) = P [(f x,p*q) | (f,p) <- fs, (x,q) <- xs]

-- For modeling events which depend on each other (like choosing
-- marbles out of a bag), we need a Monad.
instance Monad P where
    return = pure
    (P ps) >>= k = P [(v',p*q) | (v,p) <- ps, (v',q) <- unP $ k v ]
-}
