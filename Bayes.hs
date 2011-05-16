{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeOperators,
             UndecidableInstances, FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
module Bayes where

import Conditional
import Distribution
import HSet
import Utils
import Variable

import Data.List
import qualified Data.Map as M

-- heterogenous set of HP's and HC's make up a network
newtype (HSet s) => Network s = Network { dists :: s }
    deriving (Show)
-- heterogenous set of observations makes up the evidence
newtype (HSet s, Variable s) => Evidence s = Evidence s
    deriving (Show)
-- list of probabilities, with heterogeneous sets as keys
newtype (HSet vs, Variable vs) => Factor vs = Factor [(vs,Float)]
    deriving (Show)

noEvidence = Evidence HTip

-- The main function
elim :: ( Variable x, Variable e, HNotMember x e
        , HFoldr FoldWithVarsHCUnion HTip n s
        , HDiff s (HAdd x e) hs
        , HFoldr (FoldNetworkToFactors e) HTip n fs
        , HFoldr SumoutHiddens fs hs fs'
        , Variable s', HSet s'
        , HFoldr FoldPointwise (Factor HTip) fs' (Factor s')
        , HSubtype s' (Singleton x), HMember x s')
     => x -> Evidence e -> Network n -> HP (Singleton x)
elim x (Evidence e) n = toHP $ foldPointwise reduced
    where -- note: it's probably wrong to have the factor list be
          -- type-indexed. Should be a normal HList. Reason: intermediate
          -- factors generated during the summing out process could have
          -- the same types, but will get combined immediately. This
          -- causes some queries to fail at compile time erroneously
          --
          -- factors :: fs
          factors = hFoldr (FoldNetworkToFactors e) hEmpty (dists n)
          -- hiddens :: hs
          hiddens = (vars n) `hDiff` (x .>. e)
          -- reduced :: fs'
          reduced = hFoldr SumoutHiddens factors hiddens

-- | Remove evidence (via hProject) and normalize
toHP :: (Variable a, HSet a, HSubtype a a') => Factor a -> HP a'
toHP (Factor as) = HC (\htip -> normalize [(hProject a,p) | (a,p) <- as])

-- Find the vars in a network.
vars :: (HFoldr FoldWithVarsHCUnion HTip n s') => Network n -> s'
vars n = hFoldr FoldWithVarsHCUnion HTip (dists n)

data FoldWithVarsHCUnion = FoldWithVarsHCUnion
instance ( HUnion a b s', HSet s', HUnion s s' s'')
        => HFoldOp FoldWithVarsHCUnion (HC a b) s s'' where
    hFoldOp _ e s = s .+. (varsHC e)

-- Holy context explosion batman!
-- Convert a network (list of conditional probabilities to a list of factors
data FoldNetworkToFactors es = FoldNetworkToFactors es
instance ( Variable a, Variable b, Variable vs, Variable es, Variable a', Variable b'
         , HUnion a b vs
         , HIntersection es a s', HUnion s' a a', HSubtype a' a
         , HIntersection es b s'', HUnion s'' b b', HSubtype b' b
         , HEqual a a, HEqual b b, HNotMember (Factor vs) s
         , HSet es, HSet s', HSet s'', HSet vs)
        => HFoldOp (FoldNetworkToFactors es) (HC a b) s (HAdd (Factor vs) s) where
    hFoldOp (FoldNetworkToFactors es) hc factors = (mkFactor hc (Evidence es)) .>. factors

-- Find only those factors that contain a hidden variable
data Contains h = Contains h
instance (HElem h s b) => HFilterOp (Contains h) (Factor s) b where
    hFilterOp _ _ = fst $ hElem (undefined :: h) (witness :: s)

-- Used when summing out a variable to 'set' the variable to a specific value
data SetAll a = SetAll a
instance (Variable a, Variable as, HSet as, HElem a as b, HNotMember (Factor as) s)
        => HFoldOp (SetAll a) (Factor as) s (HAdd (Factor as) s) where
    hFoldOp (SetAll a) factor factors = (set a factor) .>. factors

set :: (Variable a, Variable as, HSet as, HBool b, HElem a as b) => a -> Factor as -> Factor as
set a (Factor as) = Factor [ (v,p) | (v,p) <- as, let (tf,a') = hElem a v, true tf, a == a' ]

-- Sum out a hidden variable.
data SumoutHiddens = SumoutHiddens
instance ( Variable h, Variable s, Variable s', HSet fs, HSet fs'
         , HFilter (Contains h) fs fs', HDiff fs fs' fs''
         , HFoldr (SetAll h) HTip fs' fs'''
         , HFoldr FoldPointwise (Factor HTip) fs''' (Factor s)
         , HNotMember (Factor s') fs'', HDelete h s s', HEqual s' s')
        => HFoldOp SumoutHiddens h fs (HAdd (Factor s') fs'') where
    hFoldOp _ h fs = (sumout h fs') .>. fs''
        where fs' = hFilter (Contains h) fs -- factors that contain h
              fs'' = hDiff fs fs'           -- factors that don't contain h

-- for every possible value of the hidden variable, compute the pointwise project
-- of the factors containing that hidden variable, then add them together
sumout :: forall h fs fs' s s'.
           ( Variable h, Variable s, Variable s'
           , HSet fs
           , HFoldr (SetAll h) HTip fs fs'
           , HFoldr FoldPointwise (Factor HTip) fs' (Factor s)
           , HDelete h s s', HEqual s' s')
        => h -> fs -> Factor s'
sumout h fs = foldr1 pointwiseA withouth
    where withouth = [ Factor [ (hDelete h v,p) | (v,p) <- f ] | Factor f <- products ]
          products = [ foldPointwise $ hFoldr (SetAll v) hEmpty fs | v <- domain :: [h]]

-- fold the factors with pointwise multiplication
data FoldPointwise = FoldPointwise
instance ( Variable bs, Variable abs, Variable as
         , HUnion as bs abs, HIntersection bs as s', HIntersection as bs s, HEqual s s'
         , HSet bs, HSet abs, HSet as)
        => HFoldOp FoldPointwise (Factor as) (Factor bs) (Factor abs) where
    hFoldOp _ as bs = pointwiseM as bs
foldPointwise fs = hFoldr FoldPointwise pointwiseMident fs

-- Turn a conditional distribution into a factor
mkFactor :: forall a b e s s' a' b' vs.
            ( Variable a, Variable b, Variable e, Variable a', Variable b'
            , HSet a, HSet b, HSet e, HSet s, HSet s', HSet a', HSet b'
            , HIntersection e a s, HUnion s a a', HSubtype a' a, HEqual a a
            , HIntersection e b s', HUnion s' b b', HSubtype b' b, HEqual b b
            , Variable vs, HSet vs, HUnion a b vs)
         => HC a b -> Evidence e -> Factor vs
mkFactor (HC f) e = Factor [(i .+. o,p) | i <- nubBy (.==.) $ map (conditionOn e) (domain :: [a])
                                        , (o,p) <- f i
                                        , o .==. (conditionOn e o)]

-- condition entries on the evidence values
conditionOn :: forall e v s v'.
               (HSet e, Variable e, HSet v, HIntersection e v s, HUnion s v v', HSubtype v' v)
                 => Evidence e -> v -> v
conditionOn (Evidence e) v = hProject $ (e .*. v) .+. v

-- pointwise multiplication of factors (see pg 510 of AI: A Modern Approach 2nd ed)
-- a kind of union-based multiplication
pointwiseM :: ( HSet as, HSet bs, HSet abs
              , Variable as, Variable bs, Variable abs
              , HIntersection as bs s, HIntersection bs as s', HEqual s s'
              , HUnion as bs abs)
           => Factor as -> Factor bs -> Factor abs
pointwiseM (Factor as) (Factor bs) = Factor [ (a .+. b, p * q)
                                            | (a,p) <- as
                                            , (b,q) <- bs
                                            , (a .*. b) .==. (b .*. a) ]

-- identity for fold of pointwise multiplication
pointwiseMident :: Factor HTip
pointwiseMident = Factor [(hEmpty,1.0)]

-- pointwise addition of two factors, used in summing out process
pointwiseA :: (HSet as, Variable as, HEqual as as)
           => Factor as -> Factor as -> Factor as
pointwiseA (Factor as) (Factor bs) = Factor [ (a, p + q)
                                            | (a,p) <- as
                                            , (b,q) <- bs
                                            , a .==. b ]
