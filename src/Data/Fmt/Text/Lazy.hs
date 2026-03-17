{-# LANGUAGE LambdaCase #-}

-- | Lazy 'TL.Text' as @Mu (Cons StrictText)@.
--
-- Lazy Text is internally a linked list of strict chunks:
--
-- @
-- data Text = Empty | Chunk !StrictText Text
-- @
--
-- This is exactly @Fix (Cons StrictText)@. This module provides
-- conversions and operations that let you apply the full recursion
-- scheme and streaming metamorphism infrastructure to lazy Text.
module Data.Fmt.Text.Lazy (
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

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal.Lazy as TLI
import Data.Functor.Fixed (Mu, Nu (..), unwrap, Algebra, Coalgebra)
import Data.Functor.Foldable (fold, unfold, refold, comap)
import Data.Functor.Pattern (Cons (..))

---------------------------------------------------------------------
-- Conversion
---------------------------------------------------------------------

-- | View a lazy 'TL.Text' as @Mu (Cons Text)@.
--
-- Each 'Cons' layer holds one strict chunk.
--
-- O(n) — rebuilds the spine as a Church-encoded list.
toChunks :: TL.Text -> Mu (Cons Text)
toChunks = unfold coalg
  where
    coalg lt = case lt of
        TLI.Empty -> Nil
        TLI.Chunk c rest -> Cons c rest

-- | Reassemble a @Mu (Cons Text)@ into a lazy 'TL.Text'.
--
-- O(n) — folds the Church-encoded list back into chunks.
fromChunks :: Mu (Cons Text) -> TL.Text
fromChunks = fold alg
  where
    alg Nil = TL.empty
    alg (Cons c rest) = TLI.Chunk c rest

-- | View a lazy 'TL.Text' as @Nu (Cons Text)@ for lazy streaming.
-- O(1) construction — the Text itself is the seed.
toNuChunks :: TL.Text -> Nu (Cons Text)
toNuChunks = Nu coalg
  where
    coalg lt = case lt of
        TLI.Empty -> Nil
        TLI.Chunk c rest -> Cons c rest

---------------------------------------------------------------------
-- Folding
---------------------------------------------------------------------

-- | Fold a lazy 'TL.Text' chunk by chunk.
--
-- @foldChunks alg = fold alg . toChunks@
foldChunks :: Algebra (Cons Text) a -> TL.Text -> a
foldChunks alg = fold alg . toChunks

---------------------------------------------------------------------
-- Unfolding
---------------------------------------------------------------------

-- | Build a lazy 'TL.Text' from a seed, one chunk at a time.
--
-- @unfoldChunks coalg = fromChunks . unfold coalg@
unfoldChunks :: Coalgebra (Cons Text) a -> a -> TL.Text
unfoldChunks coalg = fromChunks . unfold coalg

---------------------------------------------------------------------
-- Refold
---------------------------------------------------------------------

-- | Unfold then fold, fused — no intermediate @Mu@ structure.
refoldChunks :: Algebra (Cons Text) b -> Coalgebra (Cons Text) a -> a -> b
refoldChunks = refold

---------------------------------------------------------------------
-- Mapping
---------------------------------------------------------------------

-- | Map a function over each strict chunk.
mapChunks :: (Text -> Text) -> TL.Text -> TL.Text
mapChunks f = fromChunks . comap f . toChunks

---------------------------------------------------------------------
-- Streaming
---------------------------------------------------------------------

-- | Apply a streaming transformation to the chunks.
streamChunks
    :: (Mu (Cons Text) -> Mu (Cons Text))
    -> TL.Text -> TL.Text
streamChunks f = fromChunks . f . toChunks

-- | Apply a streaming transformation that changes the element type.
transformChunks
    :: (Mu (Cons Text) -> Mu (Cons a))
    -> TL.Text -> [a]
transformChunks f = go . unwrap . f . toChunks
  where
    go Nil = []
    go (Cons a rest) = a : go (unwrap rest)
