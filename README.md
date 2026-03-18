[![CI](https://github.com/cmk/stringfmt/actions/workflows/ci.yml/badge.svg)](https://github.com/cmk/stringfmt/actions/workflows/ci.yml)

# stringfmt

Type-safe string formatting and pretty-printing as an indexed
continuation profunctor, with document trees via Church-encoded
fixed points and recursion schemes.

## Quick start

```haskell
import Data.Fmt
import Data.Fmt.Code
import Data.Fmt.ByteString

-- Type-safe printf-style formatting
>>> runByteFmt ("Name: " % s % ", Age: " % d) "Alice" 30
"Name: Alice, Age: 30"

-- Pretty-printing with width-sensitive layout
import Data.Fmt.Tree

>>> let doc = group (vsep [short "hello", short "world"])
>>> pretty (LayoutOptions (AvailablePerLine 80 1.0)) doc
"hello world"
>>> pretty (LayoutOptions (AvailablePerLine 5 1.0)) doc
"hello\nworld"
```

## Core type

```haskell
newtype Fmt m a b = Fmt { unFmt :: (m -> a) -> b }
```

`Fmt m` is `Costar ((->) m)` from `profunctors`. The monoid `m`
accumulates formatted output, `a` is the result type, and `b`
captures arguments via indexed continuation passing.

Format holes are tracked in the type:

```haskell
type Fmt1 m s a   = Fmt m s (a -> s)        -- one hole
type Fmt2 m s a b = Fmt m s (a -> b -> s)   -- two holes
```

Formatters compose with `(%)`:

```haskell
(%) :: Semigroup m => Fmt m b c -> Fmt m a b -> Fmt m a c
```

## Output targets

Specialize `Fmt m` to different output types:

| Module | Type alias | Monoid `m` | Runner |
|---|---|---|---|
| `Data.Fmt.ByteString` | `ByteFmt` | `ByteString.Builder` | `runByteFmt` |
| `Data.Fmt.Text` | `TextFmt` | `Text.Builder` | `runTextFmt` |
| `Data.Fmt.String` | `StringFmt` | `Builder` (ShowS) | `runStringFmt` |

Each module provides combinators specialized to its output type:
`hsep`, `vsep`, `list1`, `jsonList`, `yamlList`, `jsonMap`,
`yamlMap`, `replace1`, `splitWith`, etc.

## Numeric encoders

`Data.Fmt.Code` provides Builder-based encoders following C printf
conventions:

```haskell
d   :: Fmt1 Builder s Int          -- decimal
u   :: Fmt1 Builder s Word         -- unsigned
x   :: Fmt1 Builder s Word         -- hex (lowercase)
hhx :: Fmt1 Builder s Word8        -- hex Word8
lx' :: Fmt1 Builder s Word32       -- fixed-width hex Word32
e   :: Int -> Fmt1 m s Double      -- scientific notation
f   :: Int -> Fmt1 m s Double      -- fixed-point
s   :: Show a => Fmt1 m s a        -- via Show
c   :: Fmt1 m s Char               -- single character
```

Width prefixes: `hh` (8-bit), `h` (16-bit), `l` (32-bit),
`ll` (64-bit). Primed variants (`hx'`, `lx'`) are fixed-width
(zero-padded).

## Pretty-printing

`Data.Fmt.Tree` provides a Wadler/Lindig-style pretty-printer
with width-sensitive layout.

### Document type

```haskell
type Tree m ann = Mu (Doc m ann)
```

`Tree` is the fixed point of the `Doc` pattern functor —
a document tree that can be laid out at different widths.
It is a `Semigroup`, `Monoid`, and `IsString`, so it works
as the monoid in `Fmt (Tree m ann) a b`.

### Combinators

```haskell
-- Line breaks
line, line', hardline, softline, softline' :: Tree m ann

-- Grouping and indentation
group  :: Tree m ann -> Tree m ann
nest   :: Int -> Tree m ann -> Tree m ann
align  :: Tree m ann -> Tree m ann
hang   :: Int -> Tree m ann -> Tree m ann
indent :: IsString m => Int -> Tree m ann -> Tree m ann

-- Separators
hsep, vsep, sep, fillSep :: [Tree m ann] -> Tree m ann
hcat, vcat, cat, fillCat :: [Tree m ann] -> Tree m ann

-- Enclosure
list, tupled :: [Tree m ann] -> Tree m ann
encloseSep :: Tree m ann -> Tree m ann -> Tree m ann -> [Tree m ann] -> Tree m ann

-- Filling
fill, fillBreak :: Int -> Tree m ann -> Tree m ann
width :: Tree m ann -> (Int -> Tree m ann) -> Tree m ann
```

### Layout and rendering

```haskell
-- List-based (eager)
layoutPretty  :: LayoutOptions -> Tree m ann -> [Token m ann]
layoutCompact :: Tree m ann -> [Token m ann]
render        :: (Monoid m, IsString m) => [Token m ann] -> m
pretty        :: (Monoid m, IsString m) => LayoutOptions -> Tree m ann -> m

-- Stream-based (lazy, via Nu)
layoutStream  :: LayoutOptions -> Tree m ann -> Nu (Cons (Token m ann))
renderStream  :: (Monoid m, IsString m) => Nu (Cons (Token m ann)) -> m
prettyStream  :: (Monoid m, IsString m) => LayoutOptions -> Tree m ann -> m
```

The layout algorithm supports ribbon fraction for controlling
how much of each line is filled relative to the nesting level.

### Optimization

```haskell
fuse   :: Semigroup m => Tree m ann -> Tree m ann  -- merge adjacent leaves
group' :: Tree m ann -> Tree m ann                  -- avoid unnecessary Union nodes
```

### Algebraic laws

All combinators satisfy Wadler's algebraic laws from
["A prettier printer"](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf)
(1997), verified by 21 Hedgehog properties:

```
x <> (y <> z)      = (x <> y) <> z
x <> empty         = x
empty <> x         = x
text (s ++ t)      = text s <> text t
text ""            = empty
nest (i + j) x     = nest i (nest j x)
nest 0 x           = x
nest i (x <> y)    = nest i x <> nest i y
nest i empty       = empty
nest i (text s)    = text s
nest i (align x)   = align x
group empty        = empty
group (text s <> x) = text s <> group x
group (nest i x)   = nest i (group x)
group (align x)    = align (group x)
align empty        = empty
align (text s)     = text s
align (align x)    = align x
x <$> (y <$> z)   = (x <$> y) <$> z
x <> (y <$> z)    = (x <> y) <$> z
x <$> (y <> z)    = (x <$> y) <> z
```

## Fixed points and recursion schemes

`Data.Fmt.Fixed` re-exports three fixed-point types from
[data-fix](https://hackage.haskell.org/package/data-fix) and
provides an extended recursion scheme library:

| Type | Encoding | Good at |
|---|---|---|
| `Mu f` | Church (CPS) | Folding — O(1) catamorphism |
| `Fix f` | Explicit | Pattern matching, Eq/Ord/Show/Hashable |
| `Nu f` | Existential | Unfolding — O(1) anamorphism, codata |

### Recursion schemes

```haskell
-- Basic
fold            :: (f a -> a) -> Mu f -> a
unfold          :: (a -> f a) -> a -> Mu f
refold          :: (f b -> b) -> (a -> f a) -> a -> b

-- Paramorphism / zygomorphism
foldWithContext  :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
foldWithAux     :: Functor f => Algebra f b -> (f (b, a) -> a) -> Mu f -> a

-- Generalized (via distributive laws)
foldGen         :: Functor f => Distribute f (Pair c) -> GAlgebra (Pair c) f b -> Mu f -> b
unfoldGen       :: (Functor f, Functor n, Monad n) => Distribute n f -> GCoalgebra n f b -> b -> Mu f
refoldGen       :: (Functor f, Functor n, Monad n) => Distribute f (Pair c) -> Distribute n f -> GAlgebra (Pair c) f b -> GCoalgebra n f r -> r -> b

-- Monadic
foldM           :: (Traversable f, Monad m) => AlgebraM m f a -> Mu f -> m a
refoldM         :: (Traversable f, Monad m) => AlgebraM m f b -> CoalgebraM m f a -> a -> m b

-- Apomorphism
unfoldShort     :: Functor f => (a -> f (Either (Mu f) a)) -> a -> Mu f

-- Elgot algebras
elgot           :: Functor f => Algebra f b -> (r -> Either b (f r)) -> r -> b
coelgot         :: Functor f => ((r, f b) -> b) -> Coalgebra f r -> r -> b

-- Mutual recursion
mutu            :: Functor f => (f (Pair c b) -> b) -> (f (Pair b c) -> c) -> Mu f -> c
comutu          :: Functor f => (b -> f (Either r b)) -> (r -> f (Either b r)) -> r -> Mu f

-- Pre/postpromorphisms
prepro          :: Functor f => (forall a. f a -> f a) -> Algebra f c -> Mu f -> c
postpro         :: Functor f => (forall a. f a -> f a) -> Coalgebra f r -> r -> Mu f

-- Natural transformations
hoistMu         :: (forall a. f a -> g a) -> Mu f -> Mu g
comap           :: (Bifunctor f, Functor (f a), Functor (f b)) => (a -> b) -> Mu (f a) -> Mu (f b)
contramap       :: (Bifunctor f, Functor (f a), Functor (f b)) => (a -> b) -> Mu (f a) -> Mu (f b)

-- Effectful hoist
transverse      :: (Functor f, Functor g) => (forall a. f (g a) -> g (f a)) -> Mu f -> g (Mu f)
cotransverse    :: (Functor f, Functor g) => (forall a. g (f a) -> f (g a)) -> g (Mu f) -> Mu f
```

### Streaming metamorphisms (Gibbons)

Generic over the base functor and fixed-point type:

```haskell
stream  :: Functor f => (i -> g i) -> (f o -> o) -> (state -> Maybe (f state)) -> (state -> ((state -> state) -> i -> o) -> g i -> o) -> state -> i -> o
astream :: Functor f => (i -> g i) -> (f o -> o) -> (state -> Maybe (f state)) -> (g i -> Pair (state -> state) i) -> state -> i -> o
gstream :: Functor f => (i -> g i) -> (f o -> o) -> (state -> f state) -> (state -> Maybe (f state)) -> (g i -> Maybe (Pair (state -> state) i)) -> state -> i -> o
```

`Cons`-specialized:

```haskell
fstream :: (i -> Cons b i) -> (Cons a o -> o) -> (state -> Cons a state) -> (state -> b -> state) -> (state -> Cons a state) -> state -> i -> o
```

### Pattern functor: Cons

`Data.Fmt.Cons` provides the `Cons` pattern functor for
list-like structures. `Mu (Cons a)` is a Church-encoded list;
`Nu (Cons a)` is a lazy stream (codata).

```haskell
iterate :: (a -> a) -> a -> Nu (Cons a)   -- infinite stream
repeat  :: a -> Nu (Cons a)               -- constant stream
toList  :: Mu (Cons a) -> [a]
fromList :: [a] -> Mu (Cons a)
```

Lazy `ByteString` and `Text` are `Mu (Cons chunk)` internally:

```
Mu (Cons StrictByteString)  ≅  Lazy.ByteString
Mu (Cons StrictText)        ≅  Lazy.Text
```

## Kan extensions

`Data.Fmt.Kan` connects the library to
[kan-extensions](https://hackage.haskell.org/package/kan-extensions),
making the categorical structure explicit and testable.

### Fmt and Yoneda

```
forall a. Fmt m a a  ≅  m    -- by Yoneda
```

A universally-quantified `Fmt m a a` collapses to `m`. The
whole point of `Fmt` is that it does *not* universally quantify —
the index `b` carries the argument structure.

### Fmt and Day convolution

`(%)` is Day convolution of `(->) m`. Each format hole adds
a Day factor:

```
Fmt1 m s a     ~  1-fold Day of (->) m
Fmt2 m s a b   ~  2-fold Day of (->) m
```

Day's monoidal laws give laws for `%`: associativity, unit.

### Codensity and State

```
Codensity ((->) m)              ≅  State  m
Codensity (Compose ((->) m) n)  ≅  StateT m n
```

For any `Representable u` with `Rep u = r`:

```
Codensity (Compose u n)  ≅  StateT r n
```

### Day-based structural comparison

```haskell
equalDay     :: (Foldable f, Eq1 f)  => Day f f Bool -> Bool
compareDay   :: (Foldable f, Ord1 f) => Day f f Ordering -> Ordering
recursiveEq  :: (Functor f, Foldable f, Eq1 f) => Mu f -> Mu f -> Bool
recursiveOrd :: (Functor f, Foldable f, Ord1 f) => Mu f -> Mu f -> Ordering
```

### Hoist fusion via Yoneda

```haskell
liftYonedaFix  :: Mu f -> YonedaFix f f
mapYonedaFix   :: (forall a. g a -> h a) -> YonedaFix f g -> YonedaFix f h
lowerYonedaFix :: YonedaFix f g -> Mu g
```

Multiple `hoistMu` calls fuse into a single traversal.

## Dependencies

```
base, bytestring, text, profunctors, data-fix, kan-extensions, transformers
```

`data-fix` provides `Mu`/`Fix`/`Nu` with full instance suites
(`Eq`, `Ord`, `Show`, `Read`, `Hashable`, `NFData`, `Data`).
`kan-extensions` adds exactly one package beyond what `profunctors`
already pulls in.

## Property tests

217 Hedgehog properties covering:

- Fixed-point laws (Lambek round-trips, fold fusion, hoist)
- Tree instances (Semigroup, Monoid, IsString)
- Fmt core (Yoneda round-trip, `%` associativity/unit)
- Layout correctness (group fits/breaks, nest, align, annotations)
- Wadler's algebraic laws (21 laws from the 1997 paper)
- Streaming layout agreement with list-based layout
- Fuse semantics preservation and idempotency
- Kan extension properties (Day, Yoneda, Codensity, Ran, Lan, Density, Curried)
- Numeric encoder round-trips
- ByteString/Text operation correctness
