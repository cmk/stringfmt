{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type-safe formatting as an indexed continuation profunctor.
--
-- @Fmt m a b = (m -> a) -> b@ is @Costar ((->) m)@, giving it
-- 'Category', 'Arrow', and profunctor instances for free.
--
-- @
-- person :: Fmt2 String String Int
-- person = "Person's name is " % t % ", age is " % d
--
-- runFmt person "Anne" 22
-- -- "Person's name is Anne, age is 22"
-- @
module Data.Fmt (
    -- * Type
    Fmt (..),
    runFmt,

    -- * Fmt1 / Fmt2
    Fmt1,
    Fmt2,
    Fmt3,
    fmt1,
    fmt2,
    fmt1_,
    fmt2_,
    (.%),
    cat1,

    -- * Construction
    fmt,
    (%),
    apply,
    bind,
    cat,
    refmt,

    -- * Formatting
    prefix,
    suffix,
    enclose,
    tuple,
    quotes,
    quotes',
    parens,
    braces,
    brackets,
    backticks,
    indent,

    -- * Collections
    left1,
    right1,
    either1,
    maybe1,
) where

import Control.Arrow
import Control.Category (Category ())
import Data.Profunctor
import Data.String
import qualified Control.Category as C

-- | An indexed continuation formatter.
--
-- @Fmt m a b = (m -> a) -> b@ — the monoid @m@ accumulates formatted
-- output, @a@ is the result type, and @b@ captures the arguments.
--
-- This is @Costar ((->) m)@ from @profunctors@, giving 'Profunctor',
-- 'Closed', 'Costrong', 'Cochoice', 'Category', and 'Arrow' instances.
newtype Fmt m a b = Fmt {unFmt :: (m -> a) -> b}

deriving via (Costar ((->) m) a) instance Functor (Fmt m a)
deriving via (Costar ((->) m) a) instance Applicative (Fmt m a)
deriving via (Costar ((->) m) a) instance Monad (Fmt m a)
deriving via (Costar ((->) m)) instance Profunctor (Fmt m)
deriving via (Costar ((->) m)) instance Closed (Fmt m)
deriving via (Costar ((->) m)) instance Costrong (Fmt m)
deriving via (Costar ((->) m)) instance Cochoice (Fmt m)

instance (IsString m, a ~ b) => IsString (Fmt m a b) where
    fromString = fmt . fromString

instance Semigroup m => Semigroup (Fmt1 m s a) where
    (<>) = (.%)

instance Monoid m => Monoid (Fmt1 m s a) where
    mempty = Fmt (\k _ -> k mempty)

instance Monoid m => Category (Fmt m) where
    id = fmt mempty
    (.) = (%)

instance Monoid m => Arrow (Fmt m) where
    arr f = Fmt $ \k -> f (k mempty)
    x *** y = dimap fst (,) x <*> lmap snd y

instance Monoid m => Strong (Fmt m) where
    first' x = x *** C.id
    second' x = C.id *** x

---------------------------------------------------------------------
-- Running
---------------------------------------------------------------------

-- | Run a 'Fmt'.
{-# INLINE runFmt #-}
runFmt :: Fmt m m a -> a
runFmt = flip unFmt id

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

-- | Format a constant value.
{-# INLINE fmt #-}
fmt :: m -> Fmt m a a
fmt m = Fmt ($ m)

-- | Concatenate two formatters.
infixr 0 %
{-# INLINE (%) #-}
(%) :: Semigroup m => Fmt m b c -> Fmt m a b -> Fmt m a c
f % g = f `bind` \a -> g `bind` \b -> fmt (a <> b)

-- | Apply a 'Fmt1' to a 'Fmt'.
{-# INLINE apply #-}
apply :: Fmt1 m s m -> Fmt m s a -> Fmt m s a
apply (Fmt f) (Fmt a) = Fmt (a . f)

-- | Indexed bind.
{-# INLINE bind #-}
bind :: Fmt m a1 b -> (m -> Fmt m a2 a1) -> Fmt m a2 b
bind m f = Fmt $ \k -> unFmt m (\a -> unFmt (f a) k)

-- | Concatenate a collection of formatters.
{-# INLINE cat #-}
cat :: (Monoid m, Foldable f) => f (Fmt m a a) -> Fmt m a a
cat = foldr (%) C.id

-- | Map over the formatting monoid.
{-# INLINE refmt #-}
refmt :: (m1 -> m2) -> Fmt m1 a b -> Fmt m2 a b
refmt m12 (Fmt f) = Fmt $ \a -> f (a . m12)

---------------------------------------------------------------------
-- Fmt1 / Fmt2
---------------------------------------------------------------------

-- | A unary formatter: @Fmt1 m s a ~ (m -> s) -> a -> s@
type Fmt1 m s a = Fmt m s (a -> s)

-- | A binary formatter: @Fmt2 m s a b ~ (m -> s) -> a -> b -> s@
type Fmt2 m s a b = Fmt m s (a -> b -> s)

-- | A ternary formatter.
type Fmt3 m s a b c = Fmt m s (a -> b -> c -> s)

-- | Format a value using a function @a -> m@.
{-# INLINE fmt1 #-}
fmt1 :: (a -> m) -> Fmt1 m s a
fmt1 f = Fmt $ \k -> k . f

-- | Format two values.
{-# INLINE fmt2 #-}
fmt2 :: (a -> b -> m) -> Fmt2 m s a b
fmt2 f = Fmt $ \k -> fmap k . f

-- | Ignore the input, use a constant formatter.
{-# INLINE fmt1_ #-}
fmt1_ :: Fmt m a a -> Fmt1 m a b
fmt1_ = lmap const . closed

-- | Ignore two inputs.
{-# INLINE fmt2_ #-}
fmt2_ :: Fmt m a a -> Fmt2 m a b c
fmt2_ = lmap (const . const) . (closed . closed)

-- | Concatenate two formatters, applying both to the same input.
infixr 6 .%
{-# INLINE (.%) #-}
(.%) :: Semigroup m => Fmt1 m s a -> Fmt1 m s a -> Fmt1 m s a
f .% g = Fmt $ \k a ->
    unFmt f (\b1 -> unFmt g (\b2 -> k (b1 <> b2)) a) a

-- | Format each value and concatenate.
{-# INLINE cat1 #-}
cat1 :: (Monoid m, Foldable f) => Fmt1 m m a -> Fmt1 m s (f a)
cat1 f = fmt1 $ foldMap (runFmt f)

---------------------------------------------------------------------
-- Formatting
---------------------------------------------------------------------

-- | Add the given prefix.
{-# INLINE prefix #-}
prefix :: Semigroup m => m -> Fmt m a b -> Fmt m a b
prefix s f = fmt s % f

-- | Add the given suffix.
{-# INLINE suffix #-}
suffix :: Semigroup m => m -> Fmt m a b -> Fmt m a b
suffix s f = f % fmt s

-- | Enclose with prefix and suffix.
{-# INLINE enclose #-}
enclose :: Semigroup m => Fmt m b2 c -> Fmt m a b1 -> Fmt m b1 b2 -> Fmt m a c
enclose pre suf f = pre % f % suf

-- | Format a pair in parentheses.
tuple :: (Semigroup m, IsString m) => Fmt m b c -> Fmt m a b -> Fmt m a c
tuple f1 f2 = parens $ enclose f1 f2 ", "

-- | Double quotes.
{-# INLINE quotes #-}
quotes :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
quotes = enclose "\"" "\""

-- | Single quotes.
{-# INLINE quotes' #-}
quotes' :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
quotes' = enclose "'" "'"

-- | Parentheses.
{-# INLINE parens #-}
parens :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
parens = enclose "(" ")"

-- | Braces.
{-# INLINE braces #-}
braces :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
braces = enclose "{" "}"

-- | Square brackets.
{-# INLINE brackets #-}
brackets :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
brackets = enclose "[" "]"

-- | Backticks.
{-# INLINE backticks #-}
backticks :: (Semigroup m, IsString m) => Fmt m a b -> Fmt m a b
backticks = enclose "`" "`"

-- | Indent by @n@ spaces.
{-# INLINEABLE indent #-}
indent :: (IsString m, Semigroup m) => Int -> Fmt m a b -> Fmt m a b
indent n = prefix $ fromString $ replicate n ' '

---------------------------------------------------------------------
-- Collections
---------------------------------------------------------------------

-- | Format a Left, rendering Right as empty.
{-# INLINE left1 #-}
left1 :: IsString m => Fmt1 m m a -> Fmt1 m s (Either a b)
left1 f = either1 f (fmt1 $ const "")

-- | Format a Right, rendering Left as empty.
{-# INLINE right1 #-}
right1 :: IsString m => Fmt1 m m b -> Fmt1 m s (Either a b)
right1 = either1 (fmt1 $ const "")

-- | Format an Either.
{-# INLINE either1 #-}
either1 :: Fmt1 m m a -> Fmt1 m m b -> Fmt1 m s (Either a b)
either1 l r = fmt1 $ either (runFmt l) (runFmt r)

-- | Format a Maybe with a default.
{-# INLINE maybe1 #-}
maybe1 :: m -> Fmt1 m m a -> Fmt1 m s (Maybe a)
maybe1 def f = fmt1 $ maybe def (runFmt f)
