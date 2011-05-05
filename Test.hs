{-# LANGUAGE FlexibleInstances, FlexibleContexts, ParallelListComp, TypeSynonymInstances, UndecidableInstances #-}
module Test where

import Conditional
import Distribution
import Utils
import Variable

import Control.Applicative
import Data.List
import Test.QuickCheck

main = do
    print $ uniform [1..10]

-- generating an arbitrary f would be nice
propApplicative :: P Int -> Bool
propApplicative xs = (fmap f xs) == (pure f <*> xs)
    where f x = if x == maxBound then x else succ x

propFunctor :: P Int -> Bool
propFunctor xs = xs == fmap id xs

instance (Arbitrary a, Variable a) => Arbitrary (P a) where
    arbitrary = (uniform . nub) <$> arbitrary

newtype TestPair a b = TP (C a b,P a)

instance (Variable a, Variable b) => Show (TestPair a b) where
    show (TP (c,p)) = "P:\n" ++ show p
                      ++ "\nC:\n" ++ (unlines [ show b ++ "|" ++ show a ++ " = " ++ show f
                                              | (a,_) <- unC p (), (b,f) <- unC c a])


propProd :: TestPair Int Char -> Bool
propProd (TP (bga,a)) = p == (prod d a)
    where p = prod bga a
          d = divl p a

instance (Eq a, Arbitrary (P a), Arbitrary b) => Arbitrary (TestPair a b) where
    arbitrary = do
        as <- arbitrary -- an arbitrary P a
        bs <- arbitrary -- an arbitrary list of bs
        fs <- vectorOf (length (unC as ()) * length bs) $ choose (0,1.0) -- arbitrary floats
        let c = makeC (\a -> [ (b,f) | (_,b,f) <- filter ((== a) . fst3) combos ])
            combos = zipWith (\(a,b) f -> (a,b,f))
                             [(a,b) | (a,_) <- unC as (), b <- bs]
                             fs
        return (TP (c,as))
