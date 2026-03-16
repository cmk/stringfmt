{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Fmt (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- P21–P26: Fmt core
---------------------------------------------------------------------

-- P21: runFmt (fmt x) = x (Yoneda round-trip)
prop_P21_yoneda_roundtrip :: Property
prop_P21_yoneda_roundtrip = property $ do
    s <- forAll $ Gen.string (Range.linear 0 50) Gen.alpha
    runFmt (fmt s) === s

-- P22: runFmt (f % g) = runFmt f <> runFmt g (% homomorphism)
prop_P22_percent_homo :: Property
prop_P22_percent_homo = property $ do
    s <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    t <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    runFmt (fmt s % fmt t) === s <> t

-- P23: (f % g) % h = f % (g % h) (% associativity)
prop_P23_percent_assoc :: Property
prop_P23_percent_assoc = property $ do
    s <- forAll $ Gen.string (Range.linear 0 10) Gen.alpha
    t <- forAll $ Gen.string (Range.linear 0 10) Gen.alpha
    u <- forAll $ Gen.string (Range.linear 0 10) Gen.alpha
    runFmt ((fmt s % fmt t) % fmt u) === runFmt (fmt s % (fmt t % fmt u))

-- P24: fmt mempty % f = f (% left unit)
prop_P24_percent_left_unit :: Property
prop_P24_percent_left_unit = property $ do
    s <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    runFmt (fmt mempty % fmt s) === s

-- P25: f % fmt mempty = f (% right unit)
prop_P25_percent_right_unit :: Property
prop_P25_percent_right_unit = property $ do
    s <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    runFmt (fmt s % fmt mempty) === s

-- P26: fmt1 smoke test
prop_P26_fmt1_smoke :: Property
prop_P26_fmt1_smoke = property $ do
    let f :: Fmt String String (Int -> String -> String)
        f = fmt1 show % " = " % fmt1 id
    runFmt f 42 "hello" === "42 = hello"
