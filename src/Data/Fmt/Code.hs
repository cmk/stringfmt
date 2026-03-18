-- | Numeric and binary encoding formatters.
--
-- All numeric encoders are polymorphic over @IsString m@,
-- so they work with any output type (String, Text.Builder,
-- ByteString.Builder, etc.).
--
-- Naming follows C printf conventions:
--
-- * @d@, @u@, @x@ — decimal signed, unsigned, hex
-- * @hh@, @h@, @l@, @ll@ — width prefixes (8, 16, 32, 64 bit)
-- * Primed variants (@hx'@, @lx'@) — fixed-width (zero-padded)
module Data.Fmt.Code (
    -- * Generic (IsString m)
    c,
    s,

    -- * Floating point
    e,
    f,
    g,

    -- * Decimal encodings
    d,
    hhd,
    hd,
    ld,
    lld,
    u,
    hhu,
    hu,
    lu,
    llu,

    -- * Hexadecimal encodings
    x,
    hhx,
    hhx',
    hx,
    hx',
    lx,
    lx',
    llx,
    llx',

    -- * Character encodings (Builder-specific)
    c7,
    c8,
    s7,
    s8,

    -- * Binary encodings (Builder-specific)
    b,
    b',
    hhb,
    hb,
    hb',
    lb,
    lb',
    llb,
    llb',
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (intToDigit)
import Data.Fmt.Type (Fmt1, fmt1)
import Data.Int
import Data.String (IsString, fromString)
import Data.Word
import qualified Numeric as N

---------------------------------------------------------------------
-- Generic (polymorphic over IsString m)
---------------------------------------------------------------------

-- | Format a character.
{-# INLINE c #-}
c :: IsString m => Fmt1 m s Char
c = fmt1 (fromString . pure)

-- | Format a showable value.
{-# INLINE s #-}
s :: (IsString m, Show a) => Fmt1 m s a
s = fmt1 (fromString . show)

---------------------------------------------------------------------
-- Floating point
---------------------------------------------------------------------

-- | Scientific notation with @prec@ digits of precision.
e :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
e prec = fmt1 $ fromString . flip (N.showEFloat $ Just prec) []

-- | Fixed-point notation with @prec@ digits after the decimal.
f :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
f prec = fmt1 $ fromString . flip (N.showFFloat $ Just prec) []

-- | General notation (shorter of 'e' and 'f').
g :: (IsString m, RealFloat a) => Int -> Fmt1 m s a
g prec = fmt1 $ fromString . flip (N.showGFloat $ Just prec) []

---------------------------------------------------------------------
-- Decimal encodings
---------------------------------------------------------------------

-- | Decimal encoding of an 'Int'.
{-# INLINE d #-}
d :: IsString m => Fmt1 m s Int
d = fmt1 (fromString . show)

-- | Decimal encoding of an 'Int8'.
{-# INLINE hhd #-}
hhd :: IsString m => Fmt1 m s Int8
hhd = fmt1 (fromString . show)

-- | Decimal encoding of an 'Int16'.
{-# INLINE hd #-}
hd :: IsString m => Fmt1 m s Int16
hd = fmt1 (fromString . show)

-- | Decimal encoding of an 'Int32'.
{-# INLINE ld #-}
ld :: IsString m => Fmt1 m s Int32
ld = fmt1 (fromString . show)

-- | Decimal encoding of an 'Int64'.
{-# INLINE lld #-}
lld :: IsString m => Fmt1 m s Int64
lld = fmt1 (fromString . show)

-- | Decimal encoding of a 'Word'.
{-# INLINE u #-}
u :: IsString m => Fmt1 m s Word
u = fmt1 (fromString . show)

-- | Decimal encoding of a 'Word8'.
{-# INLINE hhu #-}
hhu :: IsString m => Fmt1 m s Word8
hhu = fmt1 (fromString . show)

-- | Decimal encoding of a 'Word16'.
{-# INLINE hu #-}
hu :: IsString m => Fmt1 m s Word16
hu = fmt1 (fromString . show)

-- | Decimal encoding of a 'Word32'.
{-# INLINE lu #-}
lu :: IsString m => Fmt1 m s Word32
lu = fmt1 (fromString . show)

-- | Decimal encoding of a 'Word64'.
{-# INLINE llu #-}
llu :: IsString m => Fmt1 m s Word64
llu = fmt1 (fromString . show)

---------------------------------------------------------------------
-- Hexadecimal encodings
---------------------------------------------------------------------

-- | Shortest hex of a 'Word', lower-case.
{-# INLINE x #-}
x :: IsString m => Fmt1 m s Word
x = fmt1 (fromString . flip N.showHex "")

-- | Shortest hex of a 'Word8', lower-case.
{-# INLINE hhx #-}
hhx :: IsString m => Fmt1 m s Word8
hhx = fmt1 (fromString . flip N.showHex "")

-- | Fixed-width hex of a 'Word8' (2 nibbles).
{-# INLINE hhx' #-}
hhx' :: IsString m => Fmt1 m s Word8
hhx' = fmt1 (fromString . padHex 2)

-- | Shortest hex of a 'Word16', lower-case.
{-# INLINE hx #-}
hx :: IsString m => Fmt1 m s Word16
hx = fmt1 (fromString . flip N.showHex "")

-- | Fixed-width hex of a 'Word16' (4 nibbles).
{-# INLINE hx' #-}
hx' :: IsString m => Fmt1 m s Word16
hx' = fmt1 (fromString . padHex 4)

-- | Shortest hex of a 'Word32', lower-case.
{-# INLINE lx #-}
lx :: IsString m => Fmt1 m s Word32
lx = fmt1 (fromString . flip N.showHex "")

-- | Fixed-width hex of a 'Word32' (8 nibbles).
{-# INLINE lx' #-}
lx' :: IsString m => Fmt1 m s Word32
lx' = fmt1 (fromString . padHex 8)

-- | Shortest hex of a 'Word64', lower-case.
{-# INLINE llx #-}
llx :: IsString m => Fmt1 m s Word64
llx = fmt1 (fromString . flip N.showHex "")

-- | Fixed-width hex of a 'Word64' (16 nibbles).
{-# INLINE llx' #-}
llx' :: IsString m => Fmt1 m s Word64
llx' = fmt1 (fromString . padHex 16)

---------------------------------------------------------------------
-- Character encodings (Builder-specific)
---------------------------------------------------------------------

-- | ASCII encode a 'Char'.
{-# INLINE c7 #-}
c7 :: Fmt1 B.Builder s Char
c7 = fmt1 B.char7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'Char'.
{-# INLINE c8 #-}
c8 :: Fmt1 B.Builder s Char
c8 = fmt1 B.char8

-- | ASCII encode a 'String'.
{-# INLINE s7 #-}
s7 :: Fmt1 B.Builder s String
s7 = fmt1 B.string7

-- | Latin-1 (ISO/IEC 8859-1) encode a 'String'.
{-# INLINE s8 #-}
s8 :: Fmt1 B.Builder s String
s8 = fmt1 B.string8

---------------------------------------------------------------------
-- Binary encodings (Builder-specific)
---------------------------------------------------------------------

-- | Embed a lazy 'BL.ByteString'.
{-# INLINE b #-}
b :: Fmt1 B.Builder s BL.ByteString
b = fmt1 B.lazyByteString

-- | Embed a strict 'ByteString'.
{-# INLINE b' #-}
b' :: Fmt1 B.Builder s ByteString
b' = fmt1 B.byteString

-- | Encode a 'Word8' as a single byte.
{-# INLINE hhb #-}
hhb :: Fmt1 B.Builder s Word8
hhb = fmt1 B.word8

-- | Encode a 'Word16' little-endian.
{-# INLINE hb #-}
hb :: Fmt1 B.Builder s Word16
hb = fmt1 B.word16LE

-- | Encode a 'Word16' big-endian.
{-# INLINE hb' #-}
hb' :: Fmt1 B.Builder s Word16
hb' = fmt1 B.word16BE

-- | Encode a 'Word32' little-endian.
{-# INLINE lb #-}
lb :: Fmt1 B.Builder s Word32
lb = fmt1 B.word32LE

-- | Encode a 'Word32' big-endian.
{-# INLINE lb' #-}
lb' :: Fmt1 B.Builder s Word32
lb' = fmt1 B.word32BE

-- | Encode a 'Word64' little-endian.
{-# INLINE llb #-}
llb :: Fmt1 B.Builder s Word64
llb = fmt1 B.word64LE

-- | Encode a 'Word64' big-endian.
{-# INLINE llb' #-}
llb' :: Fmt1 B.Builder s Word64
llb' = fmt1 B.word64BE

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

-- | Pad a hex string to a fixed width with leading zeros.
padHex :: (Integral a, Show a) => Int -> a -> String
padHex width n = let hex = N.showHex n ""
                  in replicate (max 0 (width - length hex)) '0' ++ hex
