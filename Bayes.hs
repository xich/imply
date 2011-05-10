{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeOperators,
             UndecidableInstances, FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
module Bayes where

import Conditional
import Distribution
import HSet
import Variable

import Data.List
import qualified Data.Map as M

-- pointwise :: [Factor] -> HP a
pointwise factors = undefined

-- heterogenous set of HP's and HC's make up a network
newtype (HSet s) => Network s = Network { dists :: s }
-- heterogenous set of observations makes up the evidence
newtype (HSet s, Variable s) => Evidence s = Evidence s

data FoldWithVarsHCUnion = FoldWithVarsHCUnion
instance (HUnion a b s', HSet s', HUnion s s' s'') => HFoldOp FoldWithVarsHCUnion (HC a b) s s'' where hFoldOp _ e s = hUnion s (varsHC e)

vars :: (HFoldr FoldWithVarsHCUnion HTip n s') => Network n -> s'
vars n = hFoldr FoldWithVarsHCUnion HTip (dists n)

data FoldNetworkToFactors hs es = FoldNetworkToFactors hs es
instance (HBool b, HElem v hs b, Variable v, Variable hs, Variable es
         , {- not real yet -} s'' ~ HAdd v s, HNotMember v s) => HFoldOp (FoldNetworkToFactors hs es) v s s'' where
    hFoldOp (FoldNetworkToFactors hs es) v factors = let newFactor = makefactor v es
                                                         factors' = newFactor .>. factors
                                                     in if true (v `hElem` hs) then sumout v factors' else factors'

elim :: ( Variable x, HNotMember x e, Variable e
        , HSet n, HFoldr FoldWithVarsHCUnion HTip n vars
        , HFoldr (FoldNetworkToFactors hs e) HTip vars fs
        , HDiff vars (x :>: e) hs)
     => x -> Evidence e -> Network n -> HP x
elim x (Evidence e) n = pointwise $ hFoldr (FoldNetworkToFactors hiddens e) hEmpty ({- hReverse $-} vars n)
    where -- hiddens :: hs
          hiddens = (vars n) `hDiff` (x .>. e)

-- sumout :: Var -> [Factor] -> [Factor]
sumout = undefined

-- makefactor :: Var -> Events s -> Factor
-- not the real type yet
makefactor :: (Variable v, Variable e) => v -> e -> v
makefactor = const
