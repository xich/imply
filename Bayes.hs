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

data FoldWithVarsHCUnion = FoldWithVarsHCUnion
instance ( HUnion a b s', HSet s', HUnion s s' s'')
        => HFoldOp FoldWithVarsHCUnion (HC a b) s s'' where
    hFoldOp _ e s = s .+. (varsHC e)

vars :: (HFoldr FoldWithVarsHCUnion HTip n s') => Network n -> s'
vars n = hFoldr FoldWithVarsHCUnion HTip (dists n)

-- Holy context explosion batman!
data FoldNetworkToFactors es = FoldNetworkToFactors es
instance ( Variable a, Variable b, Variable vs, Variable es, Variable a', Variable b'
         , HUnion a b vs
         , HIntersection es a s', HUnion s' a a', HIso a' a
         , HIntersection es b s'', HUnion s'' b b', HIso b' b
         , HEqual a a, HEqual b b, HNotMember (Factor vs) s
         , HSet es, HSet s', HSet s'', HSet vs)
        => HFoldOp (FoldNetworkToFactors es) (HC a b) s (HAdd (Factor vs) s) where
    hFoldOp (FoldNetworkToFactors es) hc factors = (mkFactor hc (Evidence es)) .>. factors

data Contains h = Contains h
instance (HElem h s b) => HFilterOp (Contains h) (Factor s) b where
    hFilterOp _ _ = fst $ hElem (undefined :: h) (witness :: s)

data SetAll a = SetAll a
instance (Variable a, Variable as, HSet as, HElem a as b, HNotMember (Factor as) s)
        => HFoldOp (SetAll a) (Factor as) s (HAdd (Factor as) s) where
    hFoldOp (SetAll a) factor factors = (set a factor) .>. factors

set :: (Variable a, Variable as, HSet as, HBool b, HElem a as b) => a -> Factor as -> Factor as
set a (Factor as) = Factor [ (v,p) | (v,p) <- as, let (tf,a') = hElem a v, true tf, a == a' ]

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

elim :: ( Variable x, Variable e, HNotMember x e
        , HFoldr FoldWithVarsHCUnion HTip n s
        , HDiff s (HAdd x e) hs
        , HFoldr (FoldNetworkToFactors e) HTip n fs
        , HFoldr SumoutHiddens fs hs fs'
        , Variable s', HSet s'
        , HFoldr FoldPointwise (Factor HTip) fs' (Factor s')
        , Variable s'', HSet s''
        , HDiff s' e s'')
     => x -> Evidence e -> Network n -> HP s''
elim x (Evidence e) n = toHP $ rmEvidence (Evidence e) $ foldPointwise reduced
    where -- factors :: fs
          factors = hFoldr (FoldNetworkToFactors e) hEmpty (dists n)
          -- hiddens :: hs
          hiddens = (vars n) `hDiff` (x .>. e)
          -- reduced :: fs'
          reduced = hFoldr SumoutHiddens factors hiddens

toHP :: (Variable a, HSet a) => Factor a -> HP a
toHP (Factor as) = HC (\htip -> normalize as)

rmEvidence :: ( Variable e, Variable ea, Variable a
              , HSet e, HSet ea, HSet a, HDiff ea e a)
           => Evidence e -> Factor ea -> Factor a
rmEvidence (Evidence e) (Factor eas) = Factor [(hDiff ea e,p) | (ea,p) <- eas]

data FoldPointwise = FoldPointwise
instance ( Variable bs, Variable abs, Variable as
         , HUnion as bs abs, HIntersection bs as s', HIntersection as bs s, HEqual s s'
         , HSet bs, HSet abs, HSet as)
        => HFoldOp FoldPointwise (Factor as) (Factor bs) (Factor abs) where
    hFoldOp _ as bs = pointwiseM as bs
foldPointwise fs = hFoldr FoldPointwise pointwiseMident fs

-- list of probabilities, with heterogeneous sets as keys
newtype (HSet vs, Variable vs) => Factor vs = Factor [(vs,Float)]
    deriving (Show)

conditionOn :: forall e v s v'.
               (HSet e, Variable e, HSet v, HIntersection e v s, HUnion s v v', HIso v' v)
                 => Evidence e -> v -> v
conditionOn (Evidence e) v = hReorder $ (e .*. v) .+. v

mkFactor :: forall a b e s s' a' b' vs.
            ( Variable a, Variable b, Variable e, Variable a', Variable b'
            , HSet a, HSet b, HSet e, HSet s, HSet s', HSet a', HSet b'
            , HIntersection e a s, HUnion s a a', HIso a' a, HEqual a a
            , HIntersection e b s', HUnion s' b b', HIso b' b, HEqual b b
            , Variable vs, HSet vs, HUnion a b vs)
         => HC a b -> Evidence e -> Factor vs
mkFactor (HC f) e = Factor [(i .+. o,p) | i <- nubBy (.==.) $ map (conditionOn e) (domain :: [a])
                                        , (o,p) <- f i
                                        , o .==. (conditionOn e o)]

pointwiseM :: ( HSet as, HSet bs, HSet abs
              , Variable as, Variable bs, Variable abs
              , HIntersection as bs s, HIntersection bs as s', HEqual s s'
              , HUnion as bs abs)
           => Factor as -> Factor bs -> Factor abs
pointwiseM (Factor as) (Factor bs) = Factor [ (a .+. b, p * q)
                                            | (a,p) <- as
                                            , (b,q) <- bs
                                            , (a .*. b) .==. (b .*. a) ]

pointwiseMident :: Factor HTip
pointwiseMident = Factor [(hEmpty,1.0)]

pointwiseA :: (HSet as, Variable as, HEqual as as)
           => Factor as -> Factor as -> Factor as
pointwiseA (Factor as) (Factor bs) = Factor [ (a, p + q)
                                            | (a,p) <- as
                                            , (b,q) <- bs
                                            , a .==. b ]
