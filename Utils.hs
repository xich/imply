module Utils where

import Data.Maybe
import qualified Data.List as List

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z

find :: (Eq a) => a -> [(a,b)] -> b
find a = fromJust . lookup a

normalize :: [(a,Float)] -> [(a,Float)]
normalize ps = [(a,p * z) | (a,p) <- ps]
    where z = 1 / (sum $ map snd ps)

-- better name for this? ensures uniqueness of keys
-- probably a better implementation too
flatten :: Eq a => [(a,Float)] -> [(a,Float)]
flatten []              = []
flatten ps@((name,_):_) = (name, sum $ map snd vs) : flatten ps'
    where (vs,ps') = List.partition ((== name) . fst) ps
