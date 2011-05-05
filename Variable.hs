module Variable where

import Utils

class (Bounded a, Enum a, Eq a, Show a) => Variable a where
    domain :: [a]
    domain = [minBound..maxBound]

instance Variable ()
instance (Variable a, Variable b) => Variable (a,b)

