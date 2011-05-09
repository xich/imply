{-# LANGUAGE ScopedTypeVariables #-}
module Variable where

import HSet
import Utils

class (Bounded a, Enum a, Eq a, Show a) => Variable a where
    domain :: [a]
    domain = [minBound..maxBound]

instance Variable ()
instance (Variable a, Variable b) => Variable (a,b)
instance Variable Char
instance Variable Int

instance Bounded HTip where
    minBound = HTip
    maxBound = HTip
instance Enum HTip where
    fromEnum _ = 0
    toEnum 0 = HTip
instance Variable HTip
-- instance (Variable e, Variable s) => Variable (HAdd e s)

instance (Bounded e, Bounded s, HNotMember e s) => Bounded (HAdd e s) where
    minBound = minBound .>. minBound
    maxBound = maxBound .>. maxBound

instance (Bounded e, Enum e, Enum s, HNotMember e s) => Enum (HAdd e s) where
    fromEnum (HAdd e s) = eSize * (fromEnum s) + (fromEnum e)
        where eSize = fromEnum (maxBound :: e) - fromEnum (minBound :: e) + 1

    toEnum i = e .>. s
        where eSize = fromEnum (maxBound :: e) - fromEnum (minBound :: e) + 1
              s = toEnum (i `div` eSize)
              e = toEnum (i `mod` eSize)
instance (Variable e, Variable s, HNotMember e s, HShow s) => Variable (HAdd e s)

-- in order to make our Variable instance above work
instance (Bounded a, Bounded b, Enum a, Enum b) => Enum (a,b) where
    fromEnum (x,y) = bSize * (fromEnum x) + (fromEnum y)
        where bSize = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1

    toEnum i = (x,y)
        where bSize = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1
              x = toEnum (i `div` bSize)
              y = toEnum (i `mod` bSize)

