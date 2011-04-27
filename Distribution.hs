-- Compare with PFP: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
module Distribution where

import Util

import Control.Applicative
import Data.Function

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

-- Distributions
weighted :: Eq a => [(a,Float)] -> P a
weighted = P . normalize . flatten

uniform :: Eq a => [a] -> P a
uniform xs = weighted $ zip xs (repeat 1)

enum :: [Float] -> P Int
enum = weighted . zip [1..]

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
