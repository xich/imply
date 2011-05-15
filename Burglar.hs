import Bayes
import Conditional
import Distribution
import HSet
import Variable

import qualified Data.Map as M

data Burglary = B | NotB
    deriving (Bounded, Enum, Eq, Show)
instance Variable Burglary

pb :: HP (Singleton Burglary)
pb = mkP [(B,0.001),(NotB,0.999)]
-- pb = enum [0.001,0.999]

data Earthquake = E | NotE
    deriving (Bounded, Enum, Eq, Show)
instance Variable Earthquake

pe :: HP (Singleton Earthquake)
pe = mkP [(E,0.002),(NotE,0.998)]
-- pe = enum [0.002,0.998]

data Alarm = A | NotA
    deriving (Bounded, Enum, Eq, Show)
instance Variable Alarm

pa = mkTupleHC c
    where c (B,E)       = [(A,0.95) ,(NotA,0.05)]
          c (B,NotE)    = [(A,0.94) ,(NotA,0.06)]
          c (NotB,E)    = [(A,0.29) ,(NotA,0.71)]
          c (NotB,NotE) = [(A,0.001),(NotA,0.999)]

data JohnCalls = J | NotJ
    deriving (Bounded, Enum, Eq, Show)
instance Variable JohnCalls

pj = mkHC c
    where c A =    [(J,0.9) ,(NotJ,0.1)]
          c NotA = [(J,0.05),(NotJ,0.95)]

data MaryCalls = M | NotM
    deriving (Bounded, Enum, Eq, Show)
instance Variable MaryCalls

pm = mkHC c
    where c A =    [(M,0.7) ,(NotM,0.3)]
          c NotA = [(M,0.01),(NotM,0.99)]

network1 = Network $ pb .>. pa .>. pe .>. pj .>. (singleton pm)
network2 = Network $ pb .>. pa .>. (singleton pe)
network3 = Network $ pb .>. (singleton pa2) -- this works (no hiddens)
network4 = Network $ pb .>. pj2 .>. (singleton pa2)

data Alarm2 = A2 | NotA2
    deriving (Bounded, Enum, Eq, Show)
instance Variable Alarm2

pa2 = mkHC c
    where c B    = [(A2,0.95),(NotA2,0.05)]
          c NotB = [(A2,0.001),(NotA2,0.999)]

data JohnCalls2 = J2 | NotJ2
    deriving (Bounded, Enum, Eq, Show)
instance Variable JohnCalls2

pj2 = mkHC c
    where c A2 =    [(J2,0.9) ,(NotJ2,0.1)]
          c NotA2 = [(J2,0.05),(NotJ2,0.95)]

main = do
    let f = elim (undefined :: Burglary) (Evidence $ J .>. singleton M) network1
    print f
