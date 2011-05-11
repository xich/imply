import Bayes
import Conditional
import HSet
import Variable

data HeavySmoker = H | NotH
    deriving (Bounded, Enum, Eq, Show)
instance Variable HeavySmoker

data Bronchitis = B | NotB
    deriving (Bounded, Enum, Eq, Show)
instance Variable Bronchitis

data Cancer = C | NotC
    deriving (Bounded, Enum, Eq, Show)
instance Variable Cancer

data Fatigue = F | NotF
    deriving (Bounded, Enum, Eq, Show)
instance Variable Fatigue

data XrayMass = X | NotX
    deriving (Bounded, Enum, Eq, Show)
instance Variable XrayMass

ph = mkP [(H,0.1),(NotH,0.9)]
pb = mkHC c
    where c H    = [(B,0.25),(NotB,0.75)]
          c NotH = [(B,0.05),(NotB,0.95)]
pc = mkHC c
    where c H    = [(C,0.003),(NotC,0.997)]
          c NotH = [(C,0.0001),(NotC,0.9999)]
pf = mkTupleHC c
    where c (B,C)       = [(F,0.99),(NotF,0.01)]
          c (B,NotC)    = [(F,0.9),(NotF,0.1)]
          c (NotB,C)    = [(F,0.9),(NotF,0.1)]
          c (NotB,NotC) = [(F,0.1),(NotF,0.9)]
px = mkHC c
    where c C    = [(X,0.6),(NotX,0.4)]
          c NotC = [(X,0.01),(NotX,0.99)]

network = Network $ ph .>. pb .>. pc .>. pf .>. singleton px
