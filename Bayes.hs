{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeOperators,
             UndecidableInstances, FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
module Bayes where

import Conditional
import Distribution
import HSet
import Variable

import Data.List
import qualified Data.Map as M

-- heterogenous set of HP's and HC's make up a network
newtype (HSet s) => Network s = Network { dists :: s }
-- heterogenous set of observations makes up the evidence
newtype (HSet s, Variable s) => Evidence s = Evidence s
    deriving (Show)

data FoldWithVarsHCUnion = FoldWithVarsHCUnion
instance (HUnion a b s', HSet s', HUnion s s' s'') => HFoldOp FoldWithVarsHCUnion (HC a b) s s'' where hFoldOp _ e s = s .+. (varsHC e)

vars :: (HFoldr FoldWithVarsHCUnion HTip n s') => Network n -> s'
vars n = hFoldr FoldWithVarsHCUnion HTip (dists n)

data FoldNetworkToFactors hs es = FoldNetworkToFactors hs es
instance (HBool b, HElem v hs b, Variable v, Variable hs, Variable es
         , {- not real yet -} s'' ~ HAdd v s, HNotMember v s) => HFoldOp (FoldNetworkToFactors hs es) v s s'' where
    hFoldOp (FoldNetworkToFactors hs es) v factors = let newFactor = makefactor v es
                                                         factors' = newFactor .>. factors
                                                     in if (true . fst) (v `hElem` hs) then sumout v factors' else factors'

elim :: ( Variable x, HNotMember x e, Variable e
        , HSet n, HFoldr FoldWithVarsHCUnion HTip n vars
        , HFoldr (FoldNetworkToFactors hs e) HTip vars fs
        , HDiff vars (x :>: e) hs)
     => x -> Evidence e -> Network n -> HP x
elim x (Evidence e) n = pointwise $ hFoldr (FoldNetworkToFactors hiddens e) hEmpty ({- hReverse $-} vars n)
    where -- hiddens :: hs
          hiddens = (vars n) `hDiff` (x .>. e)

-- list of probabilities, with heterogeneous sets as keys
newtype (HSet vs, Variable vs) => Factor vs = Factor [(vs,Float)]
    deriving (Show)

conditionOn :: forall e v s v'.
               (HSet e, Variable e, HSet v, HIntersection e v s, HUnion s v v', HReorder v' v)
                 => Evidence e -> v -> v
conditionOn (Evidence e) v = ((e .*. v) .+. v) `hReorder` (witness :: v)

mkFactor :: forall a b e s s' a' b' vs.
            ( Variable a, Variable b, Variable e, Variable a', Variable b'
            , HSet a, HSet b, HSet e, HSet s, HSet s', HSet a', HSet b'
            , HIntersection e a s, HUnion s a a', HReorder a' a, HEqual a a
            , HIntersection e b s', HUnion s' b b', HReorder b' b, HEqual b b
            , Variable vs, HSet vs, HUnion a b vs)
         => HC a b -> Evidence e -> Factor vs
mkFactor (HC f) e = Factor [(i .+. o,p) | i <- nubBy (.==.) $ map (conditionOn e) (domain :: [a])
                                        , (o,p) <- f i
                                        , o .==. (conditionOn e o)]

point :: ( HSet as, HSet bs, HSet abs
         , Variable as, Variable bs, Variable abs
         , HIntersection as bs s, HIntersection bs as s', HEqual s s'
         , HUnion as bs abs)
      => Factor as -> Factor bs -> Factor abs
point (Factor as) (Factor bs) = Factor [ (a .+. b, p * q)
                                       | (a,p) <- as
                                       , (b,q) <- bs
                                       , (a .*. b) .==. (b .*. a) ]

-- pointwise :: [Factor] -> HP a
pointwise factors = undefined

-- sumout :: Var -> [Factor] -> [Factor]
sumout = undefined

-- makefactor :: Var -> Events s -> Factor
-- not the real type yet
makefactor :: (Variable v, Variable e) => v -> e -> v
makefactor = const
