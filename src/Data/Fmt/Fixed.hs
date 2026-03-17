{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Church-encoded fixed point with recursion schemes.
--
-- @Fix f@ is the initial @f@-algebra: it can be folded by any
-- algebra @f a -> a@ in O(1) (the fold /is/ the representation).
--
-- This module is parametric in the base functor @f@ — it works
-- with any 'Functor', not just 'Data.Fmt.Functor.FmtF'.
--
-- Recursion scheme infrastructure ported from
-- @Control.Cirklon.Patn.Scheme@ with attribution.
module Data.Fmt.Fixed (
    -- * Fixed point
    Fix (..),

    -- * Strict pair
    Pair (..),
    pairFst,
    pairSnd,
    diagonal,
    swapPair,
    uncurryPair,
    fromEither,

    -- * Algebras
    Algebra,
    Coalgebra,
    GAlgebra,
    GCoalgebra,
    AlgebraM,
    CoalgebraM,

    -- * Distributive laws
    Distribute,
    lowerAlgebra,
    lowerCoalgebra,

    -- * Algebra combinators
    zipAlgebras,

    -- * Folding
    fold,
    foldWithContext,
    foldWithAux,
    foldGen,
    foldM,

    -- * Unfolding
    unfold,
    unfoldShort,
    unfoldGen,

    -- * Refold
    refold,
    refoldGen,
    refoldM,

    -- * Elgot algebras
    elgot,
    coelgot,

    -- * Mutual recursion
    mutu,
    comutu,

    -- * Natural transformations
    hoist,
    prepro,
    postpro,
    transverse,
    cotransverse,

    -- * Wrapping and unwrapping
    wrap,
    unwrap,
) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Function (on)
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Control.Monad ((<=<), join)

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

-- | Explicit fixed point for structural comparison.
--
-- Church-encoded 'Fix' cannot be compared directly (each
-- 'unwrap' is a full fold). We convert to 'Explicit' first
-- in O(n), then compare/show structurally in O(n).
--
-- This is the Day convolution approach in disguise: 'liftEq'
-- pairs up corresponding elements from two @f@-layers —
-- exactly @Day f f Bool@ with equality as the combiner.
newtype Explicit f = Explicit (f (Explicit f))

toExplicit :: Functor f => Fix f -> Explicit f
toExplicit = fold Explicit

instance Eq1 f => Eq (Explicit f) where
    Explicit x == Explicit y = liftEq (==) x y

instance Ord1 f => Ord (Explicit f) where
    compare (Explicit x) (Explicit y) = liftCompare compare x y

instance Show1 f => Show (Explicit f) where
    showsPrec d (Explicit x) = liftShowsPrec showsPrec showList d x

instance (Functor f, Eq1 f) => Eq (Fix f) where
    (==) = (==) `on` toExplicit

instance (Functor f, Ord1 f) => Ord (Fix f) where
    compare = compare `on` toExplicit

instance (Functor f, Show1 f) => Show (Fix f) where
    showsPrec d = showsPrec d . toExplicit

---------------------------------------------------------------------
-- Strict pair
---------------------------------------------------------------------

-- | Strict pair for streaming metamorphism accumulators
-- and generalized recursion schemes.
infix 2 :!:
data Pair a b = a :!: b
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor Pair where
    bimap f g (a :!: b) = f a :!: g b

instance Bifoldable Pair where
    bifoldMap f g (a :!: b) = f a `mappend` g b

instance Bitraversable Pair where
    bitraverse f g (a :!: b) = (:!:) <$> f a <*> g b

-- | First projection.
{-# INLINE pairFst #-}
pairFst :: Pair a b -> a
pairFst (a :!: _) = a

-- | Second projection.
{-# INLINE pairSnd #-}
pairSnd :: Pair a b -> b
pairSnd (_ :!: b) = b

-- | Strict diagonal: @x -> x :!: x@.
{-# INLINE diagonal #-}
diagonal :: a -> Pair a a
diagonal x = x :!: x

-- | Swap the components.
{-# INLINE swapPair #-}
swapPair :: Pair a b -> Pair b a
swapPair (a :!: b) = b :!: a

-- | Uncurry a function over a strict 'Pair'.
{-# INLINE uncurryPair #-}
uncurryPair :: (a -> b -> c) -> Pair a b -> c
uncurryPair f (a :!: b) = f a b

-- | Collapse an @Either a a@ to @a@.
{-# INLINE fromEither #-}
fromEither :: Either a a -> a
fromEither = either id id

---------------------------------------------------------------------
-- Algebras
---------------------------------------------------------------------

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

type GAlgebra w f a = f (w a) -> a

type GCoalgebra n f a = a -> f (n a)

type AlgebraM m f a = f a -> m a

type CoalgebraM m f a = a -> m (f a)

---------------------------------------------------------------------
-- Distributive laws
---------------------------------------------------------------------

-- | A natural transformation that commutes two functors.
type Distribute f g = forall a. f (g a) -> g (f a)

-- | Lower a generalized algebra to an ordinary algebra.
--
-- Used by 'foldGen' to thread comonadic context ('Pair r')
-- through each fold step.
lowerAlgebra
    :: Functor f
    => Distribute f (Pair r)
    -> GAlgebra (Pair r) f b
    -> Algebra f (Pair r b)
lowerAlgebra k phi = fmap phi . k . fmap dup
  where
    dup p = pairFst p :!: p

-- | Lower a generalized coalgebra to an ordinary coalgebra.
--
-- Used by 'unfoldGen' to thread monadic context @n@
-- through each unfold step.
lowerCoalgebra
    :: (Functor f, Functor n, Monad n)
    => Distribute n f
    -> GCoalgebra n f b
    -> n b -> f (n b)
lowerCoalgebra k psi = fmap join . k . fmap psi

---------------------------------------------------------------------
-- Algebra combinators
---------------------------------------------------------------------

-- | Run two algebras in parallel, collecting both results in a 'Pair'.
zipAlgebras :: Functor f => Algebra f c -> Algebra f d -> Algebra f (Pair c d)
zipAlgebras f g fa = f (fmap pairFst fa) :!: g (fmap pairSnd fa)

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
-- doc for the 'Union' fallback branch).
foldWithContext :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
foldWithContext alg = snd . fold (\f -> (wrap (fmap fst f), alg f))

-- | Fold with an auxiliary fold running in parallel (zygomorphism).
--
-- The main algebra sees the auxiliary result at each recursive
-- position. Useful for 'Data.Fmt.Tree.group' where the
-- auxiliary computes whether flattening changes anything, and
-- the main algebra uses that to decide whether to wrap in
-- 'Data.Fmt.Functor.Union'.
foldWithAux :: Functor f => Algebra f b -> (f (b, a) -> a) -> Fix f -> a
foldWithAux aux alg = snd . fold (\f -> (aux (fmap fst f), alg f))

-- | Generalized fold using a distributive law (gcata).
foldGen :: Functor f => Distribute f (Pair c) -> GAlgebra (Pair c) f b -> Fix f -> b
foldGen k phi = pairSnd . fold (lowerAlgebra k phi)

-- | Monadic fold (catamorphism with effects).
--
-- Sequences effects out of the recursive positions, then
-- applies the monadic algebra.
foldM :: (Traversable f, Monad m) => AlgebraM m f a -> Fix f -> m a
foldM alg = fold (alg <=< sequenceA)

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

-- | Generalized unfold using a distributive law (gana).
unfoldGen
    :: (Functor f, Functor n, Monad n)
    => Distribute n f
    -> GCoalgebra n f b
    -> b -> Fix f
unfoldGen k psi = unfold (lowerCoalgebra k psi) . pure

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

-- | Generalized hylomorphism using distributive laws on both sides.
refoldGen
    :: (Functor f, Functor n, Monad n)
    => Distribute f (Pair c)
    -> Distribute n f
    -> GAlgebra (Pair c) f b
    -> GCoalgebra n f r
    -> r -> b
refoldGen w m phi psi = pairSnd . refold (lowerAlgebra w phi) (lowerCoalgebra m psi) . pure

-- | Monadic hylomorphism: unfold then fold with effects, fused.
refoldM :: (Traversable f, Monad m) => AlgebraM m f b -> CoalgebraM m f a -> a -> m b
refoldM alg coalg = go
  where
    go a = do
        fa <- coalg a
        fb <- traverse go fa
        alg fb

---------------------------------------------------------------------
-- Elgot algebras
---------------------------------------------------------------------

-- | Elgot algebra: unfold with short-circuit.
--
-- The coalgebra can bail out early by returning @Left b@ instead
-- of continuing to unfold via @Right (f r)@.
elgot :: Functor f => Algebra f b -> (r -> Either b (f r)) -> r -> b
elgot phi psi = go
  where
    go r = case psi r of
        Left b -> b
        Right c -> phi (fmap go c)

-- | Dual of 'elgot': the algebra receives the original seed
-- alongside the recursively-folded structure.
coelgot :: Functor f => ((r, f b) -> b) -> Coalgebra f r -> r -> b
coelgot phi psi = go
  where
    go r = phi (r, fmap go (psi r))

---------------------------------------------------------------------
-- Mutual recursion
---------------------------------------------------------------------

-- | Mutual recursion: two algebras running simultaneously.
--
-- Each algebra sees the other's result in the recursive positions
-- via 'swapPair'. The carrier is @Pair b c@, threading both results.
mutu :: Functor f => (f (Pair c b) -> b) -> (f (Pair b c) -> c) -> Fix f -> c
mutu phi' phi = pairSnd . fold (\fa ->
    let p = diagonal fa  -- Pair (f (Pair b c)) (f (Pair b c))
     in bimap (phi' . fmap swapPair) phi p)
    -- diagonal duplicates fa, then bimap applies phi' (with swapped pairs)
    -- to the first copy and phi to the second.

-- | Mutual corecursion: two coalgebras running simultaneously.
comutu :: Functor f => (b -> f (Either r b)) -> (r -> f (Either b r)) -> r -> Fix f
comutu psi' psi = unfold (fromEither . bimap (fmap swapEither . psi') psi) . Right
  where
    swapEither (Left x) = Right x
    swapEither (Right y) = Left y

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

-- | Fokkinga's prepromorphism.
--
-- Like 'fold', but applies a natural transformation at each step
-- before recursing deeper.
prepro :: Functor f => (forall a. f a -> f a) -> Algebra f c -> Fix f -> c
prepro e alg = go where go = alg . fmap (go . hoist e) . unwrap

-- | Fokkinga's postpromorphism.
--
-- Like 'unfold', but applies a natural transformation at each step
-- after unfolding.
postpro :: Functor f => (forall a. f a -> f a) -> Coalgebra f r -> r -> Fix f
postpro e coalg = go where go = wrap . fmap (hoist e . go) . coalg

-- | Effectful 'hoist': sequence effects while transforming layers.
--
-- @transverse sequenceA = pure@
transverse :: (Functor f, Functor g) => (forall a. f (g a) -> g (f a)) -> Fix f -> g (Fix f)
transverse n = fold (fmap wrap . n)

-- | Coeffectful 'hoist': transform layers while distributing a functor.
--
-- @cotransverse distAna = runIdentity@
cotransverse :: (Functor f, Functor g) => (forall a. g (f a) -> f (g a)) -> g (Fix f) -> Fix f
cotransverse n = unfold (n . fmap unwrap)

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
