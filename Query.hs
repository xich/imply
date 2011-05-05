module Query where

import Conditional
import Distribution
import Utils

import Control.Applicative
import Data.Function
import qualified Data.List as List

type Event a = a -> Bool

-- Arrows?
(??) :: Event a -> P a -> Float
(??) e = sum . map snd . filter (e . fst) . (\f -> f ()) . unC

dice :: Int -> P [Int]
dice 0 = pure []
dice n = (:) <$> (uniform [1..6]) <*> dice (n - 1)

selectOne :: Eq a => [a] -> P (a,[a])
selectOne xs = uniform [(x,List.delete x xs) | x <- xs]

selectMany :: Eq a => Int -> [a] -> P ([a],[a])
selectMany 0 xs = pure ([],xs)
selectMany n xs = do
    (y,xs') <- selectOne xs
    (ys,xs'') <- selectMany (n-1) xs'
    return (y:ys,xs'')

select :: Eq a => Int -> [a] -> P [a]
select n = fmap (reverse . fst) . selectMany n


