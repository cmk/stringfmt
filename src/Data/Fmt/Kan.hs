{-# LANGUAGE RankNTypes #-}

-- | Kan extension connections for 'Fmt' and 'Fix'.
--
-- This module makes explicit the categorical structure underlying
-- the library:
--
-- * __Day convolution__: @(%)@ is Day convolution of @(->) m@.
--   'fold2' enables parallel folds over two structures.
--   'equalDay'/'compareDay' are structural equality/ordering.
--
-- * __Yoneda__: fuses chains of 'Data.Fmt.Fixed.hoist' into a
--   single traversal. @forall a. Fmt m a a ≅ m@ is the Yoneda
--   lemma.
--
-- * __Codensity__: right-associates monadic binds for efficient
--   tree building. @Codensity ((->) m) ≅ State m@.
--
-- * __Ran/Lan__: right/left Kan extensions. @Ran u Identity a ≅
--   (Rep u, a)@ for Representable @u@ — the foundation for
--   indexed cotraversals in profunctor-optics-strings.
--
-- * __Density__: comonad from @Lan f f@ (dual of Codensity).
--
-- * __Curried__: right adjoint to Day (@Day f -| Curried f@).
--   This is what makes 'Fmt' m's 'Data.Profunctor.Closed' instance
--   work.
module Data.Fmt.Kan (
    -- * Day convolution
    equalDay,
    compareDay,
    recursiveEq,
    recursiveOrd,
    fmtDay,

    -- * Yoneda (hoist fusion)
    YonedaFix (..),
    liftYonedaFix,
    mapYonedaFix,
    lowerYonedaFix,

    -- * Codensity
    foldMCodensity,
    codensityToState,
    stateToCodensity,

    -- * Re-exports
    module Data.Functor.Day,
    module Data.Functor.Yoneda,
    module Control.Monad.Codensity,
    module Data.Functor.Kan.Ran,
    module Data.Functor.Kan.Lan,
    module Control.Comonad.Density,
    module Data.Functor.Day.Curried,
) where

import Control.Comonad.Density
import Control.Monad.Codensity
import Data.Foldable (toList)
import Data.Fmt.Cons (Cons (..))
import Data.Fmt.Fixed
import Data.Fmt.Type (Fmt (..))
import Data.Functor.Classes (Eq1 (..), Ord1 (..))
import Data.Functor.Day
import Data.Functor.Day.Curried
import Data.Functor.Kan.Lan
import Data.Functor.Kan.Ran
import Data.Functor.Yoneda

---------------------------------------------------------------------
-- Day convolution
---------------------------------------------------------------------

-- | Structural equality via Day convolution.
--
-- Pairs up two @f@-layers element-by-element and checks that:
-- 1. The shapes match (via @eqF@)
-- 2. All paired elements satisfy the combining function
--
-- This is 'liftEq' decomposed into its Day components:
-- @Day f f Bool@ is "two layers paired with a boolean combiner".
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> import Data.Functor.Classes
-- >>> equalDay (liftEq (==)) (Day (Cons 1 True) (Cons 1 True) (&&))
-- True
-- >>> import Data.Functor.Classes
-- >>> equalDay (liftEq (==)) (Day (Cons 1 True) (Cons 2 True) (&&))
-- False
--
-- __Connection:__ @Eq1 f@ is exactly the structure needed to
-- evaluate @Day f f Bool@ — it pairs elements and combines
-- with a boolean function. This is Day convolution specialized
-- to the @Bool@ monoid under @(&&)@.
equalDay :: (Foldable f, Eq1 f) => Day f f Bool -> Bool
equalDay (Day f1 f2 fn) =
    liftEq (\_ _ -> True) f1 f2
        && and (zipWith fn (toList f1) (toList f2))

-- | Structural ordering via Day convolution.
--
-- Like 'equalDay' but produces an 'Ordering'.
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> compareDay (Day (Cons 1 EQ) (Cons 2 EQ) (<>))
-- LT
--
-- __Connection:__ @Ord1 f@ provides the Day structure for
-- @Ordering@ under @(<>)@.
compareDay :: (Foldable f, Ord1 f) => Day f f Ordering -> Ordering
compareDay (Day f1 f2 fn) =
    liftCompare (\_ _ -> EQ) f1 f2
        <> mconcat (zipWith fn (toList f1) (toList f2))

-- | Recursive equality via 'fold2' and 'equalDay'.
--
-- Compares two @Mu f@ values layer by layer using Day
-- convolution. Equivalent to @(==)@ from the @Eq (Mu f)@
-- instance, but expressed explicitly via Day.
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,3])
-- True
-- >>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,4])
-- False
--
-- __Connection:__ this is @fold2 (equalDay (liftEq (==)))@ —
-- the Eq instance decomposed into its Day + fold components.
recursiveEq :: (Functor f, Foldable f, Eq1 f) => Mu f -> Mu f -> Bool
recursiveEq x y = equalDay (Day (unwrap x) (unwrap y) (\a b -> recursiveEq a b))

-- | Recursive ordering via Day convolution.
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> recursiveOrd (fromList [1,2,3 :: Int]) (fromList [1,2,4])
-- LT
recursiveOrd :: (Functor f, Foldable f, Ord1 f) => Mu f -> Mu f -> Ordering
recursiveOrd x y = compareDay (Day (unwrap x) (unwrap y) (\a b -> recursiveOrd a b))

-- | Show that @(%)@ is Day convolution of @(->) m@.
--
-- Each format hole adds a Day factor. @Fmt1@ is one Day
-- factor, @Fmt2@ is two, etc.
--
-- >>> import Data.Fmt
-- >>> import Data.Fmt.Kan
-- >>> let f = fmt "hello" :: Fmt String String String
-- >>> let g = fmt " world" :: Fmt String String String
-- >>> let d = fmtDay f g
-- >>> dap d ""
-- "hello world"
-- "hello world"
--
-- __Connection:__ @(%)@ is @dap . fmtDay@ — it constructs
-- the Day product then collapses it via the @Semigroup@ on @m@.
-- | Witness that @(%)@ is Day convolution of @(->) m@.
--
-- Decomposes two formatters into their @(->) m@ components
-- and pairs them in a Day product. 'dap' from
-- "Data.Functor.Day" recovers the combined result.
--
-- >>> import Data.Fmt
-- >>> import Data.Fmt.Kan
-- >>> dap (fmtDay (fmt "hello " :: Fmt String String String) (fmt "world")) ""
-- "hello world"
-- "hello world"
--
-- __Connection:__ each @Fmt m@ is @Costar ((->) m)@, and
-- @(%)@ combines two such via the @Semigroup@ on @m@. Day
-- convolution makes this combination explicit as a tensor
-- product of the @(->) m@ functor.
fmtDay
    :: Semigroup m
    => Fmt m m m
    -> Fmt m m m
    -> Day ((->) m) ((->) m) m
fmtDay (Fmt f) (Fmt g) = Day (\m -> f (<> m)) (\m -> g (<> m)) (<>)
  -- \m -> f (<> m) :: m -> m  (run formatter with m appended to accumulator)
  -- combiner: (<>) on the results

---------------------------------------------------------------------
-- Yoneda
---------------------------------------------------------------------

-- | Yoneda-encoded natural transformation accumulator for 'Fix'.
--
-- @YonedaFix f g@ holds a @Mu f@ together with a pending
-- natural transformation @f ~> g@. Multiple transformations
-- compose via 'mapYonedaFix' in O(1) (function composition).
-- The actual traversal happens once at 'lowerYonedaFix'.
--
-- Without Yoneda:
--
-- @
-- hoist n3 . hoist n2 . hoist n1   -- 3 full traversals
-- @
--
-- With Yoneda:
--
-- @
-- lowerYonedaFix . mapYonedaFix n3 . mapYonedaFix n2 . mapYonedaFix n1 . liftYonedaFix
-- -- 1 traversal
-- @
--
-- __Connection:__ this is the Yoneda lemma for the functor
-- category @[Hask, Hask]@: natural transformations out of
-- @f@ are equivalent to @f@ itself. Accumulating them as
-- function composition defers the cost.
data YonedaFix f g = YonedaFix (forall a. f a -> g a) (Mu f)

-- | Lift a 'Fix' into 'YonedaFix' with the identity transformation.
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> toList (lowerYonedaFix (liftYonedaFix xs))
-- [1,2,3]
liftYonedaFix :: Mu f -> YonedaFix f f
liftYonedaFix = YonedaFix id

-- | Apply a natural transformation in O(1) (just composition).
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> let inc Nil = Nil; inc (Cons a r) = Cons (a + 1) r
-- >>> toList (lowerYonedaFix (mapYonedaFix inc (liftYonedaFix xs)))
-- [2,3,4]
mapYonedaFix :: (forall a. g a -> h a) -> YonedaFix f g -> YonedaFix f h
mapYonedaFix n (YonedaFix m t) = YonedaFix (n . m) t

-- | Lower the accumulated transformation, performing a single traversal.
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> let inc Nil = Nil; inc (Cons a r) = Cons (a + 1) r
-- >>> toList (lowerYonedaFix (mapYonedaFix inc (mapYonedaFix inc (liftYonedaFix xs))))
-- [3,4,5]
lowerYonedaFix :: YonedaFix f g -> Mu g
lowerYonedaFix (YonedaFix n t) = hoistMu n t

---------------------------------------------------------------------
-- Codensity
---------------------------------------------------------------------

-- | Monadic fold accelerated by 'Codensity'.
--
-- Standard 'Data.Fmt.Fixed.foldM' can be O(n^2) on deep
-- left-nested trees because @(>>=)@ associates to the left.
-- 'Codensity' right-associates the binds:
--
-- @
-- foldMCodensity alg = lowerCodensity . foldM (Codensity . alg)
-- @
--
-- >>> import Data.Fmt.Kan
-- >>> import Data.Fmt.Cons
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> foldMCodensity (\case Nil -> pure 0; Cons a b -> pure (a + b)) xs :: Maybe Int
-- Just 6
--
-- __Connection:__ @Codensity m a = forall b. (a -> m b) -> m b@.
-- This is the CPS transform of @m@ — it right-associates binds,
-- turning O(n^2) left-nested @>>=@ into O(n).
foldMCodensity
    :: (Traversable f, Monad m)
    => AlgebraM m f a
    -> Mu f
    -> m a
foldMCodensity alg = lowerCodensity . fold go
  where
    go fa = Codensity $ \k -> do
        a <- alg =<< traverse (\(Codensity c) -> c pure) fa
        k a

-- | Convert between @Codensity ((->) m)@ and @State m@.
--
-- @Codensity ((->) m) a = forall b. (a -> m -> b) -> m -> b@
--
-- This is isomorphic to @State m a = m -> (a, m)@ — both
-- are "computations that read and transform an @m@ environment."
--
-- >>> import Data.Fmt.Kan
-- >>> codensityToState (Codensity (\k m -> k (m + 1) (m * 2))) (10 :: Int)
-- (11,20)
--
-- __Connection:__ @Codensity@ of a representable functor @(->) m@
-- gives the state monad for @m@. This is because
-- @Ran ((->) m) ((->) m) ≅ (->) m ∘ (->) m ≅ State m@.
codensityToState :: Codensity ((->) m) a -> m -> (a, m)
codensityToState (Codensity f) m = f (\a m' -> (a, m')) m

-- | Inverse of 'codensityToState'.
--
-- >>> import Data.Fmt.Kan
-- >>> stateToCodensity (\m -> (m + 1, m * 2)) `codensityToState` (10 :: Int)
-- (11,20)
stateToCodensity :: (m -> (a, m)) -> Codensity ((->) m) a
stateToCodensity f = Codensity $ \k m -> let (a, m') = f m in k a m'
