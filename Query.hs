module Query where

-- import Conditional
import HSet
import Distribution
import Utils

import Control.Applicative
import Data.Function
import qualified Data.List as List

type Event a = a -> Bool

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

uniform' :: Eq a => [a] -> P a
uniform' xs = P $ normalize $ zip xs (repeat 1)

product :: P a -> P b -> P (a,b)
product as bs = (,) <$> as <*> bs

-- Arrows?
(??) :: Event a -> P a -> Float
(??) e = sum . map snd . filter (e . fst) . unP

dice :: Int -> P [Int]
dice 0 = pure []
dice n = (:) <$> (uniform' [1..6]) <*> dice (n - 1)

diceM :: Int -> P [Int]
diceM 0 = return []
diceM n = do
    roll <- uniform' [1..6]
    rest <- diceM (n - 1)
    return $ roll:rest

selectOne :: Eq a => [a] -> P (a,[a])
selectOne xs = uniform' [(x,List.delete x xs) | x <- xs]

selectMany :: Eq a => Int -> [a] -> P ([a],[a])
selectMany 0 xs = return ([],xs)
selectMany n xs = do
    (y,xs') <- selectOne xs
    (ys,xs'') <- selectMany (n-1) xs'
    return (y:ys,xs'')

select :: Eq a => Int -> [a] -> P [a]
select n = fmap (reverse . fst) . selectMany n


