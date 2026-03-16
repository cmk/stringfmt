{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Code (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt (Fmt (..), Fmt1, fmt, fmt1, (%))
import Data.Fmt.ByteString (runByteFmt)
import Data.Fmt.Code

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower)
import Data.Int
import Data.Word
import Numeric (readHex, showHex)

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

-- | Run a raw Builder function to String (for testing Builder primitives directly).
runB :: (a -> B.Builder) -> a -> String
runB f = BL.unpack . B.toLazyByteString . f

---------------------------------------------------------------------
-- P51–P70: Code encoders
---------------------------------------------------------------------

-- P51: c formats a character
prop_P51_char :: Property
prop_P51_char = property $ do
    ch <- forAll Gen.alpha
    runByteFmt c ch === BL.singleton ch

-- P52: s formats via show
prop_P52_show :: Property
prop_P52_show = property $ do
    n <- forAll $ Gen.int (Range.linear (-1000) 1000)
    runByteFmt s n === BL.pack (show n)

-- P53: d encodes Int as decimal
prop_P53_int_dec :: Property
prop_P53_int_dec = property $ do
    n <- forAll $ Gen.int (Range.linear (-1000) 1000)
    BL.unpack (runByteFmt d n) === show n

-- P54: u encodes Word as decimal
prop_P54_word_dec :: Property
prop_P54_word_dec = property $ do
    n <- forAll $ Gen.word (Range.linear 0 10000)
    BL.unpack (runByteFmt u n) === show n

-- P55: x produces lowercase hex
prop_P55_hex_lower :: Property
prop_P55_hex_lower = property $ do
    n <- forAll $ Gen.word (Range.linear 0 10000)
    BL.unpack (runByteFmt x n) === map toLower (showHex n "")

-- P56: sized hex encoders use correct types
prop_P56_sized_hex :: Property
prop_P56_sized_hex = property $ do
    n8 <- forAll $ Gen.word8 Range.linearBounded
    n16 <- forAll $ Gen.word16 Range.linearBounded
    BL.unpack (runByteFmt hhx n8) === map toLower (showHex n8 "")
    BL.unpack (runByteFmt hx n16) === map toLower (showHex n16 "")

-- P57: fixed-width hex pads with zeros
prop_P57_hex_fixed :: Property
prop_P57_hex_fixed = property $ do
    n8 <- forAll $ Gen.word8 Range.linearBounded
    BL.length (runByteFmt hhx' n8) === 2

-- P58: e formats scientific notation
prop_P58_scientific :: Property
prop_P58_scientific = property $ do
    let result = runByteFmt (e 3) (3.14159 :: Double)
    assert $ BL.elem 'e' result || BL.elem 'E' result

-- P59: f formats fixed-point
prop_P59_fixed :: Property
prop_P59_fixed = property $ do
    BL.unpack (runByteFmt (f 2) (3.14159 :: Double)) === "3.14"

-- P60: g formats general
prop_P60_general :: Property
prop_P60_general = property $ do
    let result = runByteFmt (g 5) (3.14159 :: Double)
    assert $ BL.length result > 0

-- P61: d round-trips
prop_P61_d_roundtrip :: Property
prop_P61_d_roundtrip = property $ do
    n <- forAll $ Gen.int (Range.linear (-10000) 10000)
    (read (BL.unpack (runByteFmt d n)) :: Int) === n

-- P62: u round-trips
prop_P62_u_roundtrip :: Property
prop_P62_u_roundtrip = property $ do
    n <- forAll $ Gen.word (Range.linear 0 10000)
    (read (BL.unpack (runByteFmt u n)) :: Word) === n

-- P63: x round-trips via readHex
prop_P63_x_roundtrip :: Property
prop_P63_x_roundtrip = property $ do
    n <- forAll $ Gen.word (Range.linear 0 10000)
    let hex = BL.unpack $ runByteFmt x n
    fst (head (readHex hex)) === n

-- P64: word-size variants encode correctly
prop_P64_word_sizes :: Property
prop_P64_word_sizes = property $ do
    n8 <- forAll $ Gen.word8 Range.linearBounded
    n16 <- forAll $ Gen.word16 Range.linearBounded
    n32 <- forAll $ Gen.word32 Range.linearBounded
    n64 <- forAll $ Gen.word64 Range.linearBounded
    (read (BL.unpack (runByteFmt hhu n8)) :: Word8) === n8
    (read (BL.unpack (runByteFmt hu n16)) :: Word16) === n16
    (read (BL.unpack (runByteFmt lu n32)) :: Word32) === n32
    (read (BL.unpack (runByteFmt llu n64)) :: Word64) === n64

-- P65: b' embeds strict ByteString
prop_P65_bytestring :: Property
prop_P65_bytestring = property $ do
    str <- forAll $ Gen.string (Range.linear 0 30) Gen.alpha
    let bs = BL.toStrict (BL.pack str)
    BL.unpack (runByteFmt b' bs) === str

-- P66: b embeds lazy ByteString
prop_P66_lazy_bytestring :: Property
prop_P66_lazy_bytestring = property $ do
    str <- forAll $ Gen.string (Range.linear 0 30) Gen.alpha
    let lbs = BL.pack str
    runByteFmt b lbs === lbs

-- P67: hb vs hb' (little vs big endian)
prop_P67_endianness :: Property
prop_P67_endianness = property $ do
    n <- forAll $ Gen.filter (\w -> w `div` 256 /= w `mod` 256) $
        Gen.word16 (Range.linear 256 65535)
    let le = runByteFmt hb n
        be = runByteFmt hb' n
    assert $ le /= be

-- P68: composing encoders with %
prop_P68_compose :: Property
prop_P68_compose = property $ do
    let result = runByteFmt ("Value: " % d % ", Hex: " % x) 42 42
    result === "Value: 42, Hex: 2a"

-- P69: s7 encodes ASCII string
prop_P69_string7 :: Property
prop_P69_string7 = property $ do
    str <- forAll $ Gen.string (Range.linear 0 30) Gen.alpha
    BL.unpack (runByteFmt s7 str) === str

-- P70: int-size variants
prop_P70_int_sizes :: Property
prop_P70_int_sizes = property $ do
    n8 <- forAll $ Gen.int8 Range.linearBounded
    n16 <- forAll $ Gen.int16 Range.linearBounded
    n32 <- forAll $ Gen.int32 Range.linearBounded
    n64 <- forAll $ Gen.int64 Range.linearBounded
    (read (BL.unpack (runByteFmt hhd n8)) :: Int8) === n8
    (read (BL.unpack (runByteFmt hd n16)) :: Int16) === n16
    (read (BL.unpack (runByteFmt ld n32)) :: Int32) === n32
    (read (BL.unpack (runByteFmt lld n64)) :: Int64) === n64
