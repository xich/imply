{-# LANGUAGE FlexibleInstances, FlexibleContexts, ParallelListComp #-}
module Test where

import Distribution
import Conditional

import Control.Applicative
import Test.QuickCheck

main = do
    print $ uniform [1..10]

propApplicative :: (Num a) => P a -> Bool
propApplicative xs = (fmap f xs) == (pure f <*> xs)
    where f = (+1)

propFunctor :: (Eq a) => P a -> Bool
propFunctor xs = xs == fmap id xs

propProd :: (Eq a, Eq b) => TestPair a b -> Bool
propProd (TP (bga,a)) = bga == d
    where p = prod bga a
          d = divl p a

instance (Arbitrary a, Eq a) => Arbitrary (P a) where
    arbitrary = uniform <$> arbitrary

newtype TestPair a b = TP (C a b,P a)
    deriving (Show)

instance (Arbitrary (P a), Arbitrary b) => Arbitrary (TestPair a b) where
    arbitrary = do
        as <- arbitrary
        bs <- arbitrary
        fs <- vectorOf (length (unP as) * length bs) $ choose (0,1.0)
        let cs = C $ zipWith (\(a,b) f -> (a,b,f)) [(a,b) | (a,_) <- unP as, b <- bs] fs
        return (TP (cs,as))
