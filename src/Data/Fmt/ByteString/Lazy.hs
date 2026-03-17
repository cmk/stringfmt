{-# LANGUAGE LambdaCase #-}

-- | Lazy 'BL.ByteString' as @Mu (Cons StrictByteString)@.
--
-- Lazy ByteString is internally a linked list of strict chunks:
--
-- @
-- data ByteString = Empty | Chunk !StrictByteString ByteString
-- @
--
-- This is exactly @Fix (Cons StrictByteString)@. This module
-- provides conversions and operations that let you apply the
-- full recursion scheme and streaming metamorphism infrastructure
-- to lazy ByteStrings.
--
-- @
-- import Data.Fmt.ByteString.Lazy
--
-- \-\- Fold a lazy ByteString chunk by chunk
-- foldChunks (\\case Nil -> 0; Cons chunk n -> BS.length chunk + n) myLBS
--
-- \-\- Stream-transform chunks
-- transformChunks (fstream unwrap wrap produce consume flush state) myLBS
-- @
module Data.Fmt.ByteString.Lazy (
    -- * Conversion
    toChunks,
    fromChunks,
    toNuChunks,

    -- * Folding
    foldChunks,

    -- * Unfolding
    unfoldChunks,

    -- * Refold
    refoldChunks,

    -- * Mapping
    mapChunks,

    -- * Streaming
    streamChunks,
    transformChunks,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Functor.Fixed (Mu, Nu (..), unwrap, Algebra, Coalgebra)
import Data.Functor.Foldable (fold, unfold, refold, comap)
import Data.Functor.Pattern (Cons (..))

---------------------------------------------------------------------
-- Conversion
---------------------------------------------------------------------

-- | View a lazy 'BL.ByteString' as @Mu (Cons ByteString)@.
--
-- Each 'Cons' layer holds one strict chunk.
--
-- O(n) — rebuilds the spine as a Church-encoded list.
toChunks :: BL.ByteString -> Mu (Cons ByteString)
toChunks = unfold coalg
  where
    coalg lbs = case lbs of
        BLI.Empty -> Nil
        BLI.Chunk c rest -> Cons c rest

-- | Reassemble a @Mu (Cons ByteString)@ into a lazy 'BL.ByteString'.
--
-- O(n) — folds the Church-encoded list back into chunks.
fromChunks :: Mu (Cons ByteString) -> BL.ByteString
fromChunks = fold alg
  where
    alg Nil = BL.empty
    alg (Cons c rest) = BLI.Chunk c rest

-- | View a lazy 'BL.ByteString' as @Nu (Cons ByteString)@ for
-- lazy streaming. O(1) construction — the ByteString itself is
-- the seed.
toNuChunks :: BL.ByteString -> Nu (Cons ByteString)
toNuChunks = Nu coalg
  where
    coalg lbs = case lbs of
        BLI.Empty -> Nil
        BLI.Chunk c rest -> Cons c rest

---------------------------------------------------------------------
-- Folding
---------------------------------------------------------------------

-- | Fold a lazy 'BL.ByteString' chunk by chunk.
--
-- @foldChunks alg = fold alg . toChunks@
--
-- >>> foldChunks (\case Nil -> 0; Cons c n -> BS.length c + n) myLBS
foldChunks :: Algebra (Cons ByteString) a -> BL.ByteString -> a
foldChunks alg = fold alg . toChunks

---------------------------------------------------------------------
-- Unfolding
---------------------------------------------------------------------

-- | Build a lazy 'BL.ByteString' from a seed, one chunk at a time.
--
-- @unfoldChunks coalg = fromChunks . unfold coalg@
unfoldChunks :: Coalgebra (Cons ByteString) a -> a -> BL.ByteString
unfoldChunks coalg = fromChunks . unfold coalg

---------------------------------------------------------------------
-- Refold
---------------------------------------------------------------------

-- | Unfold then fold, fused — no intermediate @Mu@ structure.
--
-- @refoldChunks alg coalg = foldChunks alg . unfoldChunks coalg@
-- but without materializing the Church-encoded list.
refoldChunks :: Algebra (Cons ByteString) b -> Coalgebra (Cons ByteString) a -> a -> b
refoldChunks = refold

---------------------------------------------------------------------
-- Mapping
---------------------------------------------------------------------

-- | Map a function over each strict chunk.
--
-- >>> mapChunks (BS.map toUpper) myLBS
mapChunks :: (ByteString -> ByteString) -> BL.ByteString -> BL.ByteString
mapChunks f = fromChunks . comap f . toChunks

---------------------------------------------------------------------
-- Streaming
---------------------------------------------------------------------

-- | Apply a streaming transformation to the chunks.
--
-- Takes a function that operates on @Mu (Cons ByteString)@ and
-- lifts it to operate on lazy 'BL.ByteString'.
streamChunks
    :: (Mu (Cons ByteString) -> Mu (Cons ByteString))
    -> BL.ByteString -> BL.ByteString
streamChunks f = fromChunks . f . toChunks

-- | Apply a streaming transformation that changes the element type.
--
-- Useful for encoding/decoding pipelines where the chunk type changes.
transformChunks
    :: (Mu (Cons ByteString) -> Mu (Cons a))
    -> BL.ByteString -> [a]
transformChunks f = go . unwrap . f . toChunks
  where
    go Nil = []
    go (Cons a rest) = a : go (unwrap rest)
