{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | The @Cons@ pattern functor for list-like structures.
--
-- Isomorphic to @Maybe (a, b)@. @Mu (Cons a)@ is a
-- Church-encoded list, equivalent to @Logic a@ from @logict@.
--
-- This module also provides streaming metamorphisms (Gibbons)
-- that interleave production and consumption.
--
-- Ported from @Control.Cirklon.Patn.Scheme@ with attribution.
module Data.Fmt.Cons (
    -- * Pattern functor
    Cons (..),
    XNor,

    -- * Combinators
    toCons,
    fromCons,
    consFst,
    consSnd,
    isCons,
    isNil,

    -- * Eliminators
    elim,

    -- * Filtering
    conses,
    filterNils,

    -- * Folding
    foldCons,
    gatherCons,

    -- * Partitioning
    partitionCons,
    mapCons,

    -- * Currying & uncurrying
    consCurry,
    consUncurry,

    -- * Distributivity
    pairCons,
    unpairWith,

    -- * Associativity
    reassocLR,
    reassocRL,

    -- * Symmetry
    swapCons,

    -- * Showing
    showsPrecF,

    -- * List conversion
    toList,
    fromList,

    -- * Distributive laws (Cons-specialized)
    distAna,
    distCata,
    distTuple,
    distEither,
    seqEither,

    -- * Cons-specialized type aliases
    ConsAlgebra,
    ConsCoalgebra,
    ConsGAlgebra,
    ConsGCoalgebra,
    ConsAlgebraM,
    ConsCoalgebraM,

    -- * Streaming metamorphisms (Cons-specialized)
    fstream,

    -- * Nu-based iteration
    iterate,
    repeat,
) where

import Control.Applicative (Alternative (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Fmt.Fixed
import Data.Function ((&))
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.Functor.Identity (Identity (..), runIdentity)
import GHC.Show (showList__)
import Prelude hiding (iterate, repeat)

---------------------------------------------------------------------
-- Pattern functor
---------------------------------------------------------------------

-- | The pattern functor for list-like structures.
--
-- Isomorphic to @Maybe (a, b)@. @Mu (Cons a)@ is a
-- Church-encoded list.
data Cons a b = Nil | Cons !a b
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor Cons where
    bimap _ _ Nil = Nil
    bimap f g (Cons a b) = Cons (f a) (g b)

instance Bifoldable Cons where
    bifoldMap _ _ Nil = mempty
    bifoldMap f g (Cons a b) = f a `mappend` g b

instance Bitraversable Cons where
    bitraverse _ _ Nil = pure Nil
    bitraverse f g (Cons a b) = Cons <$> f a <*> g b

instance Monoid a => Applicative (Cons a) where
    pure = Cons mempty
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons a f <*> Cons c d = Cons (a `mappend` c) (f d)

instance Monoid a => Monad (Cons a) where
    Nil >>= _ = Nil
    Cons a b >>= k = case k b of
        Nil -> Nil
        Cons c d -> Cons (a `mappend` c) d

instance (Semigroup a, Semigroup b) => Semigroup (Cons a b) where
    Nil <> b = b
    a <> Nil = a
    Cons a b <> Cons c d = Cons (a <> c) (b <> d)

instance (Semigroup a, Semigroup b) => Monoid (Cons a b) where
    mempty = Nil

instance Eq a => Eq1 (Cons a) where
    liftEq _ Nil Nil = True
    liftEq eq (Cons a1 b1) (Cons a2 b2) = a1 == a2 && eq b1 b2
    liftEq _ _ _ = False

instance Ord a => Ord1 (Cons a) where
    liftCompare _ Nil Nil = EQ
    liftCompare _ Nil _ = LT
    liftCompare _ _ Nil = GT
    liftCompare cmp (Cons a1 b1) (Cons a2 b2) = compare a1 a2 <> cmp b1 b2

instance Show a => Show1 (Cons a) where
    liftShowsPrec _ _ _ Nil = showString "Nil"
    liftShowsPrec sp _ d (Cons a b) = showParen (d > 10) $
        showString "Cons " . showsPrec 11 a . showChar ' ' . sp 11 b

-- | @XNor@ is a synonym for 'Cons' (the name used by Gibbons).
type XNor = Cons

---------------------------------------------------------------------
-- Showing
---------------------------------------------------------------------

-- | Show a single layer of a 'Show1' functor whose recursive
-- positions carry @Int -> ShowS@ continuations.
showsPrecF :: Show1 f => Int -> f (Int -> ShowS) -> ShowS
showsPrecF = liftShowsPrec (&) (showList__ ($ 0))

---------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------

-- | Convert a 'Maybe' value into a 'Cons' value.
toCons :: Maybe (a, b) -> Cons a b
toCons Nothing = Nil
toCons (Just (a, b)) = Cons a b

-- | Convert a 'Cons' value into a 'Maybe' value.
fromCons :: Cons a b -> Maybe (a, b)
fromCons Nil = Nothing
fromCons (Cons a b) = Just (a, b)

-- | Project the first component.
consFst :: Cons a b -> Maybe a
consFst Nil = Nothing
consFst (Cons a _) = Just a

-- | Project the second component.
consSnd :: Cons a b -> Maybe b
consSnd Nil = Nothing
consSnd (Cons _ b) = Just b

-- | Detect whether a 'Cons' value is not empty.
isCons :: Cons a b -> Bool
isCons = not . isNil

-- | Detect whether a 'Cons' value is empty.
isNil :: Cons a b -> Bool
isNil Nil = True
isNil _ = False

---------------------------------------------------------------------
-- Eliminators
---------------------------------------------------------------------

-- | Case elimination for 'Cons'.
elim :: c -> (a -> b -> c) -> Cons a b -> c
elim c _ Nil = c
elim _ f (Cons a b) = f a b

---------------------------------------------------------------------
-- Filtering
---------------------------------------------------------------------

-- | Collect the @(a, b)@ pairs from a 'Foldable' of 'Cons' values.
conses :: Foldable f => f (Cons a b) -> [(a, b)]
conses = foldr go []
  where
    go (Cons a b) acc = (a, b) : acc
    go _ acc = acc

-- | Filter out 'Nil' values.
filterNils :: Foldable f => f (Cons a b) -> [Cons a b]
filterNils = foldr go []
  where
    go Nil acc = acc
    go a acc = a : acc

---------------------------------------------------------------------
-- Folding
---------------------------------------------------------------------

-- | Fold over the 'Cons' cases with an accumulating function.
foldCons :: Foldable f => (a -> b -> m -> m) -> m -> f (Cons a b) -> m
foldCons f = foldr go
  where
    go (Cons a b) acc = f a b acc
    go _ acc = acc

-- | Zip two lists inside a 'Cons' product.
gatherCons :: Cons [a] [b] -> [Cons a b]
gatherCons (Cons as bs) = zipWith Cons as bs
gatherCons _ = []

---------------------------------------------------------------------
-- Partitioning
---------------------------------------------------------------------

-- | Partition a 'Foldable' of 'Cons' values into alternatives.
partitionCons
    :: (Foldable t, Alternative f)
    => t (Cons a b) -> (f a, f b)
partitionCons = foldr go (empty, empty)
  where
    go Nil acc = acc
    go (Cons a b) (as, bs) = (pure a <|> as, pure b <|> bs)

-- | Partition by mapping into 'Cons'.
mapCons
    :: (Alternative f, Traversable t)
    => (a -> Cons b c) -> t a -> (f b, f c)
mapCons f = partitionCons . fmap f

---------------------------------------------------------------------
-- Currying & uncurrying
---------------------------------------------------------------------

-- | Curry a function on 'Cons' to pointed arguments.
consCurry :: (Cons a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
consCurry f (Just a) (Just b) = f (Cons a b)
consCurry _ _ _ = Nothing

-- | Uncurry a function on pointed arguments to 'Cons'.
consUncurry :: (Maybe a -> Maybe b -> Maybe c) -> Cons a b -> Maybe c
consUncurry _ Nil = Nothing
consUncurry f (Cons a b) = f (Just a) (Just b)

---------------------------------------------------------------------
-- Distributivity
---------------------------------------------------------------------

-- | Distribute the second component of a 'Cons' over a pair.
pairCons :: Cons c (a, b) -> (Cons c a, Cons c b)
pairCons Nil = (Nil, Nil)
pairCons (Cons c (a, b)) = (Cons c a, Cons c b)

-- | Merge two 'Cons' values with a combining function.
unpairWith :: (c -> c -> c) -> Cons c a -> Cons c b -> Cons c (a, b)
unpairWith f (Cons c1 a) (Cons c2 b) = Cons (f c1 c2) (a, b)
unpairWith _ _ _ = Nil

---------------------------------------------------------------------
-- Associativity
---------------------------------------------------------------------

-- | Reassociate left to right.
reassocLR :: Cons (Cons a b) c -> Cons a (Cons b c)
reassocLR (Cons (Cons a b) c) = Cons a (Cons b c)
reassocLR _ = Nil

-- | Reassociate right to left.
reassocRL :: Cons a (Cons b c) -> Cons (Cons a b) c
reassocRL (Cons a (Cons b c)) = Cons (Cons a b) c
reassocRL _ = Nil

---------------------------------------------------------------------
-- Symmetry
---------------------------------------------------------------------

-- | Swap the two components.
swapCons :: Cons a b -> Cons b a
swapCons Nil = Nil
swapCons (Cons a b) = Cons b a

---------------------------------------------------------------------
-- List conversion
---------------------------------------------------------------------

-- | Convert a 'Mu (Cons a)' to a Haskell list.
toList :: Mu (Cons a) -> [a]
toList = fold (elim [] (:))

-- | Convert a Haskell list to a 'Mu (Cons a)'.
fromList :: [a] -> Mu (Cons a)
fromList = foldr (\a r -> wrap (Cons a r)) (wrap Nil)

---------------------------------------------------------------------
-- Cons-specialized type aliases
---------------------------------------------------------------------

type ConsAlgebra a b = Cons a b -> b

type ConsCoalgebra a b = b -> Cons a b

type ConsGAlgebra w a b = Cons a (w b) -> b

type ConsGCoalgebra n a b = b -> Cons a (n b)

type ConsAlgebraM m a b = Cons a b -> m b

type ConsCoalgebraM m a b = b -> m (Cons a b)

---------------------------------------------------------------------
-- Distributive laws (Cons-specialized)
---------------------------------------------------------------------

-- | Trivial distribution for 'unfold': @Identity@ carries no context.
distAna :: Distribute Identity (Cons a)
distAna = fmap Identity . runIdentity

-- | Trivial distribution for 'fold': @Identity@ carries no context.
distCata :: Distribute (Cons a) Identity
distCata = Identity . fmap runIdentity

-- | Distribution for 'foldWithAux': pairs each recursive position
-- with an auxiliary result computed by the helper algebra.
distTuple :: (Cons a b -> b) -> Distribute (Cons a) (Pair b)
distTuple phi = bimap (phi . fmap pairFst) (fmap pairSnd) . diagonal

-- | Distribution for 'unfoldShort': the @Left@ branch short-circuits.
distEither :: (b -> Cons a b) -> Distribute (Either b) (Cons a)
distEither psi = either (fmap Left . psi) (fmap Right)

-- | Equivalent to 'distEither', collapsing via 'fromEither'.
seqEither :: (b -> Cons a b) -> Distribute (Either b) (Cons a)
seqEither psi = fromEither . bimap (fmap Left . psi) (fmap Right)

---------------------------------------------------------------------
-- Streaming metamorphisms (Cons-specialized)
--
-- The generic stream/astream/gstream live in Data.Fmt.Fixed.
-- fstream adds the Cons-specific element decomposition.
---------------------------------------------------------------------

-- | Gibbons' streaming metamorphism, specialized to 'Cons'.
--
-- Parameterized by produce, consume, and flush. Transforms
-- an input structure into an output by interleaving:
--
-- * @produce@: try to emit output from the accumulator state
-- * @consume@: fold the next input element into the state
-- * @flush@: drain remaining state when input is exhausted
--
-- Generic over the fixed-point types @i@ and @o@:
--
-- @
-- fstream unwrap wrap produce consume flush  -- Mu -> Mu
-- fstream unwrapNu wrapMu produce consume flush  -- Nu -> Mu
-- @
fstream
    :: (i -> Cons b i)          -- ^ project input
    -> (Cons a o -> o)          -- ^ embed output
    -> (state -> Cons a state)  -- ^ @produce@: unfold output from state
    -> (state -> b -> state)    -- ^ @consume@: fold an input element into state
    -> (state -> Cons a state)  -- ^ @flush@: drain remaining state
    -> state -> i -> o
fstream proj emb produce consume flush =
    gstream proj emb flush
        (\s -> case produce s of
            Nil -> Nothing
            other -> Just other)
        (\case
            Nil -> Nothing
            Cons a x' -> Just (flip consume a :!: x'))

---------------------------------------------------------------------
-- Nu-based iteration
---------------------------------------------------------------------

-- | Apply a pure function ad infinitum, producing an infinite stream.
--
-- @iterate f x = [x, f x, f (f x), ...]@
--
-- Uses 'Nu' (greatest fixed point) because the result is codata —
-- a potentially infinite stream that is consumed lazily.
iterate :: (a -> a) -> a -> Nu (Cons a)
iterate f = Nu (\a -> Cons a (f a))

-- | Repeat a value ad infinitum.
--
-- @repeat x = [x, x, x, ...]@
repeat :: a -> Nu (Cons a)
repeat = iterate id
