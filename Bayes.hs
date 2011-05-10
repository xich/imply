{-# LANGUAGE ExistentialQuantification #-}
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
newtype (HSet s) => Network s = Network { vars :: s }
-- heterogenous set of observations makes up the evidence
newtype (HSet s, Variable s) => Evidence s = Evidence s

elim :: (Variable x, HSet n) => x -> Evidence e -> Network n -> HP x
elim x ev bn = pointwise $ go ({- hreverse $-} vars bn) hEmpty
    where -- go :: [Var] -> [Factor] -> HSet Factor -- obviously not the actual type, but keep in mind
          go = undefined
{-
          go HTip       factors = factors
          go (HAdd e s) factors = go s (if true (e `hElem` hiddens) then sumout e factors' else factors')
            where factors' = (makefactor e ev) .>. factors
-}

          hiddens = undefined -- vars bn \\ (x : map fst ev)

-- sumout :: Var -> [Factor] -> [Factor]
sumout = undefined

-- makefactor :: Var -> Events s -> Factor
makefactor = undefined
