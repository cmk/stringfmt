{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Text (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt (Fmt (..), Fmt1, fmt, fmt1, (%))
import Data.Fmt.Text
import Data.String (fromString)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

txt :: String -> Builder
txt = fromText . T.pack

txtFmt1 :: Fmt1 Builder T.Text T.Text
txtFmt1 = fmt1 fromText

---------------------------------------------------------------------
-- P41–P50: Text
---------------------------------------------------------------------

-- P41: runTextFmt (fmt b) = toLazyText b
prop_P41_run_fmt :: Property
prop_P41_run_fmt = property $ do
    s <- forAll $ Gen.string (Range.linear 0 30) Gen.alpha
    let b = txt s
    runTextFmt (fmt b) === toLazyText b

-- P42: runTextFmt (fromString s) = pack s
prop_P42_fromstring :: Property
prop_P42_fromstring = property $ do
    s <- forAll $ Gen.string (Range.linear 0 30) Gen.alpha
    runTextFmt (fromString s :: TextFmt TL.Text TL.Text) === TL.pack s

-- P43: replace1 for Text
prop_P43_replace1 :: Property
prop_P43_replace1 = property $ do
    let input = fmt (fromText "foobarbaz")
        result = runTextFmt (replace1 "bar" (fmt (fromText "FOO")) input)
    result === "fooFOObaz"

-- P44: replace1 no match
prop_P44_replace1_no_match :: Property
prop_P44_replace1_no_match = property $ do
    let input = fmt (fromText "foobarbaz")
        result = runTextFmt (replace1 "xyz" (fmt (fromText "!!!")) input)
    result === "foobarbaz"

-- P45: hsep for Text
prop_P45_hsep :: Property
prop_P45_hsep = property $ do
    let result = run1 (hsep txtFmt1) ["one", "two", "three"]
    result === "one two three"

-- P46: vsep for Text
prop_P46_vsep :: Property
prop_P46_vsep = property $ do
    let result = run1 (vsep txtFmt1) ["a", "b"]
    result === "a\nb"

-- P47: jsonList for Text
prop_P47_jsonList :: Property
prop_P47_jsonList = property $ do
    let result = run1 (jsonList txtFmt1) ["one", "two"]
    result === "[one, two]"

-- P48: yamlList for Text
prop_P48_yamlList :: Property
prop_P48_yamlList = property $ do
    let result = run1 (yamlList txtFmt1) ["one", "two"]
    result === "- one\n- two\n"

-- P49: jsonMap for Text
prop_P49_jsonMap :: Property
prop_P49_jsonMap = property $ do
    let result = run1 (jsonMap txtFmt1 txtFmt1) [("k1", "v1"), ("k2", "v2")]
    result === "{k1: v1, k2: v2}"

-- P50: yamlMap for Text
prop_P50_yamlMap :: Property
prop_P50_yamlMap = property $ do
    let result = run1 (yamlMap txtFmt1 txtFmt1) [("k1", "v1"), ("k2", "v2")]
    result === "k1: v1\nk2: v2\n"
