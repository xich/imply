{-# LANGUAGE FlexibleInstances, FlexibleContexts, ParallelListComp #-}
module Test where

import Conditional
import Distribution
import Util

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
propProd (TP (bga,a)) = p == (prod d a)
    where p = prod bga a
          d = divl p a

instance (Arbitrary a, Eq a) => Arbitrary (P a) where
    arbitrary = uniform <$> arbitrary

newtype TestPair a b = TP (C a b,P a)

instance (Eq a, Show a, Show b) => Show (TestPair a b) where
    show (TP (c,p)) = "P:\n" ++ show p
                      ++ "\nC:\n" ++ (unlines [ show b ++ "|" ++ show a ++ " = " ++ show f
                                              | (a,_) <- unP p, (b,f) <- unC c a])

instance (Eq a, Arbitrary (P a), Arbitrary b) => Arbitrary (TestPair a b) where
    arbitrary = do
        as <- arbitrary -- an arbitrary P a
        bs <- arbitrary -- an arbitrary list of bs
        fs <- vectorOf (length (unP as) * length bs) $ choose (0,1.0) -- arbitrary floats
        let c = makeC (\a -> [ (b,f) | (_,b,f) <- filter ((== a) . fst3) combos ])
            combos = zipWith (\(a,b) f -> (a,b,f))
                             [(a,b) | (a,_) <- unP as, b <- bs]
                             fs
        return (TP (c,as))
