{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.String (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt.String
import Data.String (fromString)

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- P27–P30: String Builder
---------------------------------------------------------------------

-- P27: show (fromString s) = s (IsString round-trip via Show)
prop_P27_isstring_roundtrip :: Property
prop_P27_isstring_roundtrip = property $ do
    s <- forAll $ Gen.string (Range.linear 0 50) Gen.alpha
    show (fromString s :: Builder) === s

-- P28: show (x <> y) = show x ++ show y
prop_P28_semigroup_homo :: Property
prop_P28_semigroup_homo = property $ do
    s <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    t <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    let x = fromString s :: Builder
        y = fromString t :: Builder
    show (x <> y) === show x ++ show y

-- P29: show mempty = ""
prop_P29_monoid_empty :: Property
prop_P29_monoid_empty = property $ do
    show (mempty :: Builder) === ""

-- P30: unBuilder (fromString s) = (s ++)
prop_P30_shows_semantics :: Property
prop_P30_shows_semantics = property $ do
    s <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    t <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    unBuilder (fromString s) t === s ++ t
