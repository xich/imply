import Bayes
import Conditional
import Distribution
import HSet
import Variable

import qualified Data.Map as M

data A = A | NotA
    deriving (Bounded, Enum, Eq, Show)
instance Variable A

data B = B | NotB
    deriving (Bounded, Enum, Eq, Show)
instance Variable B

data C = C | NotC
    deriving (Bounded, Enum, Eq, Show)
instance Variable C

pb = mkHC c
    where c A    = [(B,0.3),(NotB,0.7)]
          c NotA = [(B,0.9),(NotB,0.1)]

pc = mkHC c
    where c B    = [(C,0.2),(NotC,0.8)]
          c NotB = [(C,0.6),(NotC,0.4)]

fb e = mkFactor pb e
fc e = mkFactor pc e
