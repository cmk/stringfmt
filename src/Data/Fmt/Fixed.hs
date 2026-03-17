{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

-- | Fixed points and recursion schemes.
--
-- Re-exports 'Mu', 'Fix', and 'Nu' from @data-fix@, plus an
-- extended recursion scheme library (paramorphisms, zygomorphisms,
-- Elgot algebras, mutual recursion, pre/postpromorphisms,
-- streaming, etc.).
--
-- * 'Mu' — Church-encoded least fixed point (efficient folding)
-- * 'Fix' — Explicit fixed point (pattern matching, Eq/Ord/Show/Hashable)
-- * 'Nu' — Existential greatest fixed point (efficient unfolding, codata)
--
-- Recursion scheme infrastructure ported from
-- @Control.Cirklon.Patn.Scheme@ with attribution.
module Data.Fmt.Fixed (
    -- * Fixed points (from data-fix)
    Mu (..),
    Fix (..),
    Nu (..),

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

    -- * Streaming metamorphisms
    stream,
    astream,
    gstream,

    -- * Wrapping and unwrapping
    wrap,
    unwrap,
) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Fix (Fix (..))
import Data.Fix (Mu (..), Nu (..))
import qualified Data.Fix as F
import Control.Monad ((<=<), join)

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
lowerAlgebra
    :: Functor f
    => Distribute f (Pair r)
    -> GAlgebra (Pair r) f b
    -> Algebra f (Pair r b)
lowerAlgebra k phi = fmap phi . k . fmap dup
  where
    dup p = pairFst p :!: p

-- | Lower a generalized coalgebra to an ordinary coalgebra.
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
fold :: Algebra f a -> Mu f -> a
fold = F.foldMu

-- | Fold with access to the original subtree at each position
-- (paramorphism).
foldWithContext :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
foldWithContext alg = snd . fold (\f -> (wrap (fmap fst f), alg f))

-- | Fold with an auxiliary fold running in parallel (zygomorphism).
foldWithAux :: Functor f => Algebra f b -> (f (b, a) -> a) -> Mu f -> a
foldWithAux aux alg = snd . fold (\f -> (aux (fmap fst f), alg f))

-- | Generalized fold using a distributive law (gcata).
foldGen :: Functor f => Distribute f (Pair c) -> GAlgebra (Pair c) f b -> Mu f -> b
foldGen k phi = pairSnd . fold (lowerAlgebra k phi)

-- | Monadic fold (catamorphism with effects).
foldM :: (Traversable f, Monad m) => AlgebraM m f a -> Mu f -> m a
foldM alg = fold (alg <=< sequenceA)

---------------------------------------------------------------------
-- Unfolding
---------------------------------------------------------------------

-- | Build a structure from a seed one layer at a time (anamorphism).
unfold :: Functor f => Coalgebra f a -> a -> Mu f
unfold = F.unfoldMu

-- | Build from a seed with early termination (apomorphism).
unfoldShort :: Functor f => (a -> f (Either (Mu f) a)) -> a -> Mu f
unfoldShort coalg = go where go = wrap . fmap (either id go) . coalg

-- | Generalized unfold using a distributive law (gana).
unfoldGen
    :: (Functor f, Functor n, Monad n)
    => Distribute n f
    -> GCoalgebra n f b
    -> b -> Mu f
unfoldGen k psi = unfold (lowerCoalgebra k psi) . pure

---------------------------------------------------------------------
-- Refold
---------------------------------------------------------------------

-- | Unfold then fold, fused — no intermediate structure is allocated
-- (hylomorphism).
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
mutu :: Functor f => (f (Pair c b) -> b) -> (f (Pair b c) -> c) -> Mu f -> c
mutu phi' phi = pairSnd . fold (\fa ->
    let p = diagonal fa
     in bimap (phi' . fmap swapPair) phi p)

-- | Mutual corecursion: two coalgebras running simultaneously.
comutu :: Functor f => (b -> f (Either r b)) -> (r -> f (Either b r)) -> r -> Mu f
comutu psi' psi = unfold (fromEither . bimap (fmap swapEither . psi') psi) . Right
  where
    swapEither (Left x) = Right x
    swapEither (Right y) = Left y

---------------------------------------------------------------------
-- Natural transformations
---------------------------------------------------------------------

-- | Change the base functor via a natural transformation.
-- Does not require @Functor f@.
{-# INLINE hoist #-}
hoist :: (forall a. f a -> g a) -> Mu f -> Mu g
hoist = F.hoistMu

-- | Fokkinga's prepromorphism.
prepro :: Functor f => (forall a. f a -> f a) -> Algebra f c -> Mu f -> c
prepro e alg = go where go = alg . fmap (go . hoist e) . unwrap

-- | Fokkinga's postpromorphism.
postpro :: Functor f => (forall a. f a -> f a) -> Coalgebra f r -> r -> Mu f
postpro e coalg = go where go = wrap . fmap (hoist e . go) . coalg

-- | Effectful 'hoist': sequence effects while transforming layers.
transverse :: (Functor f, Functor g) => (forall a. f (g a) -> g (f a)) -> Mu f -> g (Mu f)
transverse n = fold (fmap wrap . n)

-- | Coeffectful 'hoist': transform layers while distributing a functor.
cotransverse :: (Functor f, Functor g) => (forall a. g (f a) -> f (g a)) -> g (Mu f) -> Mu f
cotransverse n = unfold (n . fmap unwrap)

---------------------------------------------------------------------
-- Streaming metamorphisms (Gibbons)
--
-- Generic over the base functor f. A streaming metamorphism
-- interleaves production (unfold) and consumption (fold) so
-- that output can be emitted before all input is consumed.
--
-- These take project/embed functions as parameters, so they
-- work with any fixed-point type (Mu, Fix, Nu) or even plain
-- Haskell types like lists.
---------------------------------------------------------------------

-- | Core streaming metamorphism engine.
--
-- At each step: try to produce output from the current state
-- via @process@. If that succeeds, embed the output and continue.
-- If it fails, project the next input layer via @accum@.
--
-- Generic over:
-- * @g@ — input base functor (projected via first argument)
-- * @f@ — output base functor (embedded via second argument)
-- * @i@, @o@ — input/output types (any fixed point, or plain types)
stream
    :: Functor f
    => (i -> g i)                      -- ^ project input
    -> (f o -> o)                      -- ^ embed output
    -> (state -> Maybe (f state))      -- ^ @process@: try to produce
    -> (state -> ((state -> state) -> i -> o) -> g i -> o)
                                       -- ^ @accum@: consume next input
    -> state -> i -> o
stream proj emb process accum = go
  where
    go state input =
        maybe
            (accum state (\f -> go (f state)) (proj input))
            (emb . fmap (`go` input))
            $ process state

-- | Streaming anamorphism: accumulator always consumes.
astream
    :: Functor f
    => (i -> g i)                      -- ^ project input
    -> (f o -> o)                      -- ^ embed output
    -> (state -> Maybe (f state))      -- ^ @process@: try to produce
    -> (g i -> Pair (state -> state) i)
                                       -- ^ @accum@: consume, return state update + rest
    -> state -> i -> o
astream proj emb process accum = stream proj emb process $
    \_state cont -> uncurryPair cont . accum

-- | Streaming generalized apomorphism: when input is exhausted,
-- drain the remaining state via a flush coalgebra.
gstream
    :: Functor f
    => (i -> g i)                      -- ^ project input
    -> (f o -> o)                      -- ^ embed output
    -> (state -> f state)              -- ^ @flush@: drain remaining state
    -> (state -> Maybe (f state))      -- ^ @process@: try to produce
    -> (g i -> Maybe (Pair (state -> state) i))
                                       -- ^ @accum@: try to consume
    -> state -> i -> o
gstream proj emb flush process accum = stream proj emb process $
    \state cont -> maybe (drain emb flush state) (uncurryPair cont) . accum
  where
    drain e coalg = go where go = e . fmap go . coalg



-- | Inject one layer into the fixed point.
{-# INLINE wrap #-}
wrap :: Functor f => f (Mu f) -> Mu f
wrap = F.wrapMu

-- | Peel off one layer (Lambek's lemma).
unwrap :: Functor f => Mu f -> f (Mu f)
unwrap = F.unwrapMu
