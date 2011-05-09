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
pb = enum [0.001,0.999]

data Earthquake = E | NotE
    deriving (Bounded, Enum, Eq, Show)
instance Variable Earthquake

pe :: HP (Singleton Earthquake)
pe = enum [0.002,0.998]

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

{-
net :: Network String
net = Network (M.fromList [("B",pb),("E",pe)]) (M.fromList [("A",pa),("J",pj),("M",pm)])
-}
