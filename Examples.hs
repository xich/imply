module Examples where

import Distribution
import Query

import Control.Applicative

twoGT6 n = ((>=2) . length . filter (==6)) ?? dice n

data RGB = R | G | B
    deriving (Show, Eq)
rgb rgbs = (==[R,G,B]) ?? select 3 rgbs

-- Monty Hall, need a Monad here?
data Result = Win | Lose
    deriving (Show, Eq)

initialChoice = uniform [Win,Lose,Lose]

switch :: Result -> Result
switch Win = Lose
switch Lose = Win
