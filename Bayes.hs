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

elim :: ( Variable x, HNotMember x e, Variable e
        , HSet n, HFoldr FoldWithVarsHCUnion HTip n vars
        , HDiff vars (x :>: e) hiddens) => x -> Evidence e -> Network n -> HP x
elim x (Evidence e) n = pointwise $ go ({- hreverse $-} vars n) hEmpty
    where -- go :: [Var] -> [Factor] -> HSet Factor -- obviously not the actual type, but keep in mind
          go = undefined
{-
          go HTip       factors = factors
          go (HAdd e s) factors = go s (if true (e `hElem` hiddens) then sumout e factors' else factors')
            where factors' = (makefactor e ev) .>. factors
-}
          -- hiddens :: hiddens
          hiddens = (vars n) `hDiff` (x .>. e)

-- sumout :: Var -> [Factor] -> [Factor]
sumout = undefined

-- makefactor :: Var -> Events s -> Factor
makefactor = undefined
