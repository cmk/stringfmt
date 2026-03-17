{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Fixed (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt.Fixed

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Generators
---------------------------------------------------------------------

-- | Generate a Fix (ListF Int) tree of bounded depth.
-- ListF is a simple test functor: ListF a r = NilF | ConsF a r
data ListF a r = NilF | ConsF a r
    deriving (Functor)

type List a = Mu (ListF a)

nil :: List a
nil = wrap NilF

cons :: a -> List a -> List a
cons a xs = wrap (ConsF a xs)

-- | Fold a List into a Haskell list.
toList :: List a -> [a]
toList = fold $ \case
    NilF -> []
    ConsF a as -> a : as

-- | Build a List from a Haskell list.
fromList :: [a] -> List a
fromList = foldr cons nil

genList :: Gen (List Int)
genList = fromList <$> Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))

forAllList :: Gen (List Int) -> PropertyT IO (List Int)
forAllList = forAllWith (show . toList)

---------------------------------------------------------------------
-- P1–P8: Fix laws
---------------------------------------------------------------------

-- P1: wrap . unwrap = id (Lambek)
prop_P01_wrap_unwrap :: Property
prop_P01_wrap_unwrap = property $ do
    xs <- forAllList genList
    toList (wrap (unwrap xs)) === toList xs

-- P2: unwrap . wrap = id (Lambek)
prop_P02_unwrap_wrap :: Property
prop_P02_unwrap_wrap = property $ do
    xs <- forAll (Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100)))
    let layer = case xs of
            [] -> NilF
            (a : as) -> ConsF a (fromList as)
    toList (wrap layer) === xs

-- P3: fold alg . wrap = alg . fmap (fold alg) (fusion)
prop_P03_fold_fusion :: Property
prop_P03_fold_fusion = property $ do
    xs <- forAllList genList
    let alg :: ListF Int Int -> Int
        alg NilF = 0
        alg (ConsF a n) = a + n
        lhs = fold alg (wrap (unwrap xs))
        rhs = alg (fmap (fold alg) (unwrap xs))
    lhs === rhs

-- P4: hoistMu id = id
prop_P04_hoist_id :: Property
prop_P04_hoist_id = property $ do
    xs <- forAllList genList
    toList (hoistMu id xs) === toList xs

-- P5: hoistMu (n . m) = hoistMu n . hoistMu m
prop_P05_hoist_compose :: Property
prop_P05_hoist_compose = property $ do
    xs <- forAllList genList
    let n :: ListF Int r -> ListF Int r
        n NilF = NilF
        n (ConsF a r) = ConsF (a + 1) r
        m :: ListF Int r -> ListF Int r
        m NilF = NilF
        m (ConsF a r) = ConsF (a * 2) r
    toList (hoistMu (n . m) xs) === toList (hoistMu n (hoistMu m xs))

-- P6: refold alg coalg = fold alg . unfold coalg
prop_P06_refold_coherence :: Property
prop_P06_refold_coherence = property $ do
    xs <- forAll (Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100)))
    let alg :: ListF Int Int -> Int
        alg NilF = 0
        alg (ConsF a n) = a + n
        coalg :: [Int] -> ListF Int [Int]
        coalg [] = NilF
        coalg (a : as) = ConsF a as
    refold alg coalg xs === fold alg (unfold coalg xs)

-- P7: fold wrap = id
prop_P07_fold_wrap :: Property
prop_P07_fold_wrap = property $ do
    xs <- forAllList genList
    toList (fold wrap xs) === toList xs

-- P8: unfold unwrap = id
prop_P08_unfold_unwrap :: Property
prop_P08_unfold_unwrap = property $ do
    xs <- forAllList genList
    toList (unfold unwrap xs) === toList xs
