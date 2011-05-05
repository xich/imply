{-# LANGUAGE ExistentialQuantification #-}
module Bayes where

import Distribution
import Conditional
import Variable

import Data.List
import qualified Data.Map as M

data Network k = forall a b. (Variable a, Variable b) => Network (M.Map k (Node k (C a) b))

data Node k p a = Node [k] [k] (p a)

vars :: Network k -> [k]
vars (Network m) = M.keys m

data Factor = forall a b. (Variable a, Variable b) => Factor [(a,b,Float)]

instance Show Factor where
    show (Factor fs) = unlines [show b ++ "|" ++ show a ++ ": " ++ show p | (a,b,p) <- fs]

mkFactor :: (Variable a, Variable b) => Event b -> C a b -> Factor
mkFactor (_,x) (C f) = Factor [(a,b,p) | a <- domain, (b,p) <- f a, b == x]

type Var = String

pointwise :: [Factor] -> P a
pointwise factors = undefined

type Event a = (Var,a)

elim :: Var -> [Event a] -> Network Var -> P a
elim x ev bn = pointwise $ go (reverse $ vars bn) []
    where go :: [Var] -> [Factor] -> [Factor]
          go (v:vs) factors = go vs (if v `elem` hiddens then sumout v factors' else factors')
            where factors' = makefactor v ev : factors

          hiddens = vars bn \\ (x : map fst ev)

sumout :: Var -> [Factor] -> [Factor]
sumout = undefined

makefactor :: Var -> [Event a] -> Factor
makefactor = undefined
