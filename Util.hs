module Util where

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z

