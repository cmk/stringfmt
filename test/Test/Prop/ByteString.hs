{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.ByteString (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt (Fmt (..), Fmt1, fmt, fmt1, (%))
import Data.Fmt.ByteString
import Data.String (fromString)

import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

bs :: String -> Builder
bs = byteString . B.pack

bsFmt1 :: Fmt1 Builder B.ByteString B.ByteString
bsFmt1 = fmt1 byteString

---------------------------------------------------------------------
-- P31–P40: ByteString
---------------------------------------------------------------------

-- P31: runByteFmt (fmt b) = toLazyByteString b
prop_P31_run_fmt :: Property
prop_P31_run_fmt = property $ do
    s <- forAll $ Gen.string (Range.linear 0 30) Gen.alpha
    let b = bs s
    runByteFmt (fmt b) === toLazyByteString b

-- P32: runByteFmt (fromString s) encodes correctly
prop_P32_fromstring :: Property
prop_P32_fromstring = property $ do
    s <- forAll $ Gen.string (Range.linear 0 30) Gen.alpha
    runByteFmt (fromString s :: ByteFmt BL.ByteString BL.ByteString) === BL.pack (map (toEnum . fromEnum) s)

-- P33: replace1 substitutes first occurrence
prop_P33_replace1 :: Property
prop_P33_replace1 = property $ do
    let input = fmt (byteString "foobarbaz")
        result = runByteFmt (replace1 "bar" (fmt (byteString "FOO")) input)
    result === "fooFOObaz"

-- P34: replace1 no match leaves unchanged
prop_P34_replace1_no_match :: Property
prop_P34_replace1_no_match = property $ do
    let input = fmt (byteString "foobarbaz")
        result = runByteFmt (replace1 "xyz" (fmt (byteString "!!!")) input)
    result === "foobarbaz"

-- P35: hsep joins with spaces
prop_P35_hsep :: Property
prop_P35_hsep = property $ do
    let result = run1 (hsep bsFmt1) ["one", "two", "three"]
    result === "one two three"

-- P36: vsep joins with newlines
prop_P36_vsep :: Property
prop_P36_vsep = property $ do
    let result = run1 (vsep bsFmt1) ["a", "b"]
    result === "a\nb\n"

-- P37: jsonList wraps in brackets
prop_P37_jsonList :: Property
prop_P37_jsonList = property $ do
    let result = run1 (jsonList bsFmt1) ["one", "two"]
    result === "[one, two]"

-- P38: yamlList prefixes with "- "
prop_P38_yamlList :: Property
prop_P38_yamlList = property $ do
    let result = run1 (yamlList bsFmt1) ["one", "two"]
    result === "- one\n- two\n"

-- P39: jsonMap wraps in braces
prop_P39_jsonMap :: Property
prop_P39_jsonMap = property $ do
    let result = run1 (jsonMap bsFmt1 bsFmt1) [("k1", "v1"), ("k2", "v2")]
    result === "{k1: v1, k2: v2}"

-- P40: yamlMap uses newlines
prop_P40_yamlMap :: Property
prop_P40_yamlMap = property $ do
    let result = run1 (yamlMap bsFmt1 bsFmt1) [("k1", "v1"), ("k2", "v2")]
    result === "k1: v1\nk2: v2\n"
