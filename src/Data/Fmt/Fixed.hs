{-# LANGUAGE RankNTypes #-}

-- | Church-encoded fixed point with recursion schemes.
--
-- @Fix f@ is the initial @f@-algebra: it can be folded by any
-- algebra @f a -> a@ in O(1) (the fold /is/ the representation).
--
-- This module is parametric in the base functor @f@ — it works
-- with any 'Functor', not just 'Data.Fmt.Functor.FmtF'.
module Data.Fmt.Fixed (
    -- * Fixed point
    Fix (..),

    -- * Algebras
    Algebra,
    Coalgebra,
    GAlgebra,

    -- * Folding
    fold,
    foldWithContext,
    foldWithAux,

    -- * Unfolding
    unfold,
    unfoldShort,

    -- * Refold
    refold,

    -- * Natural transformations
    hoist,

    -- * Wrapping and unwrapping
    wrap,
    unwrap,
) where

---------------------------------------------------------------------
-- Fixed point
---------------------------------------------------------------------

-- | Church-encoded fixed point.
--
-- @
-- fold alg (Fix f) = f alg
-- @
--
-- This is the Church encoding of the least fixed point, sometimes
-- called @Mu@ in the literature. We use @Fix@ because it serves
-- the same role as the standard @Fix f = Fix (f (Fix f))@ — the
-- representation differs (CPS vs explicit recursion) but the
-- universal property is the same.
newtype Fix f = Fix {unFix :: forall a. (f a -> a) -> a}

---------------------------------------------------------------------
-- Algebras
---------------------------------------------------------------------

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

type GAlgebra w f a = f (w a) -> a

---------------------------------------------------------------------
-- Folding
---------------------------------------------------------------------

-- | Fold a structure one layer at a time (catamorphism).
--
-- O(1) dispatch — the fold /is/ the representation.
{-# INLINE fold #-}
fold :: Algebra f a -> Fix f -> a
fold alg (Fix f) = f alg

-- | Fold with access to the original subtree at each position
-- (paramorphism).
--
-- The algebra receives both the recursive result and the
-- original subtree, useful when you need the unmodified
-- structure (e.g. 'Data.Fmt.Tree.group' needs the original
-- doc for the 'UnionF' fallback branch).
foldWithContext :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
foldWithContext alg = snd . fold (\f -> (wrap (fmap fst f), alg f))

-- | Fold with an auxiliary fold running in parallel (zygomorphism).
--
-- The main algebra sees the auxiliary result at each recursive
-- position. Useful for 'Data.Fmt.Tree.group' where the
-- auxiliary computes whether flattening changes anything, and
-- the main algebra uses that to decide whether to wrap in
-- 'Data.Fmt.Functor.UnionF'.
foldWithAux :: Functor f => Algebra f b -> (f (b, a) -> a) -> Fix f -> a
foldWithAux aux alg = snd . fold (\f -> (aux (fmap fst f), alg f))

---------------------------------------------------------------------
-- Unfolding
---------------------------------------------------------------------

-- | Build a structure from a seed one layer at a time (anamorphism).
unfold :: Functor f => Coalgebra f a -> a -> Fix f
unfold coalg = go where go = wrap . fmap go . coalg

-- | Build from a seed with early termination (apomorphism).
--
-- The coalgebra can return @Left t@ to stop with a complete
-- structure, or @Right seed@ to continue building.
unfoldShort :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
unfoldShort coalg = go where go = wrap . fmap (either id go) . coalg

---------------------------------------------------------------------
-- Refold
---------------------------------------------------------------------

-- | Unfold then fold, fused — no intermediate structure is
-- allocated (hylomorphism).
--
-- This is the workhorse for the layout algorithm:
-- unfold a document tree into a token stream and fold
-- into the output format in a single pass.
{-# INLINE refold #-}
refold :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
refold alg coalg = go where go = alg . fmap go . coalg

---------------------------------------------------------------------
-- Natural transformations
---------------------------------------------------------------------

-- | Change the base functor via a natural transformation.
-- Does not require @Functor f@.
--
-- Used for e.g. stripping or transforming annotations without
-- a full fold/wrap round-trip.
{-# INLINE hoist #-}
hoist :: (forall a. f a -> g a) -> Fix f -> Fix g
hoist n (Fix mk) = Fix $ \roll -> mk (roll . n)

---------------------------------------------------------------------
-- Wrapping and unwrapping
---------------------------------------------------------------------

-- | Inject one layer into the fixed point.
{-# INLINE wrap #-}
wrap :: Functor f => f (Fix f) -> Fix f
wrap fa = Fix $ \alg -> alg (fmap (fold alg) fa)

-- | Peel off one layer (Lambek's lemma).
unwrap :: Functor f => Fix f -> f (Fix f)
unwrap = fold (fmap wrap)
