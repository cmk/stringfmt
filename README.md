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
type Tree m ann = Mu (FmtF m ann)
```

`Tree` is the fixed point of the `FmtF` pattern functor —
a document tree that can be laid out at different widths.
It is a `Semigroup`, `Monoid`, and `IsString`, so it works
as the monoid in `Fmt (Tree m ann) a b`.

### Combinators

```haskell
-- Line breaks
line, line', hardline, softline, softline' :: Tree m ann

-- Grouping and indentation
group, nest, align, hang, indent :: ... -> Tree m ann

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
nest (i + j) x     = nest i (nest j x)
nest i (text s)    = text s
group (nest i x)   = nest i (group x)
align (align x)    = align x
x <$> (y <$> z)   = (x <$> y) <$> z
...
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
fold            :: (f a -> a) -> Mu f -> a
foldWithContext  :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
foldWithAux     :: Functor f => (f b -> b) -> (f (b, a) -> a) -> Mu f -> a
foldGen         :: ... -> Mu f -> b               -- generalized (gcata)
foldM           :: ... -> Mu f -> m a             -- monadic
unfold          :: (a -> f a) -> a -> Mu f
unfoldShort     :: (a -> f (Either (Mu f) a)) -> a -> Mu f
unfoldGen       :: ... -> b -> Mu f               -- generalized (gana)
refold          :: (f b -> b) -> (a -> f a) -> a -> b
refoldGen       :: ... -> r -> b                  -- generalized (ghylo)
refoldM         :: ... -> a -> m b                -- monadic
elgot           :: ... -> r -> b                  -- unfold with short-circuit
coelgot         :: ... -> r -> b                  -- fold with original seed
mutu            :: ... -> Mu f -> c               -- mutual recursion
comutu          :: ... -> r -> Mu f               -- mutual corecursion
prepro          :: ... -> Mu f -> c               -- prepromorphism
postpro         :: ... -> r -> Mu f               -- postpromorphism
hoistMu         :: (forall a. f a -> g a) -> Mu f -> Mu g
comap           :: (Bifunctor f, ...) => (a -> b) -> Mu (f a) -> Mu (f b)
contramap       :: (Bifunctor f, ...) => (a -> b) -> Mu (f a) -> Mu (f b)
```

### Streaming metamorphisms (Gibbons)

Generic over the base functor and fixed-point type:

```haskell
stream  :: Functor f => (i -> g i) -> (f o -> o) -> ... -> state -> i -> o
astream :: Functor f => (i -> g i) -> (f o -> o) -> ... -> state -> i -> o
gstream :: Functor f => (i -> g i) -> (f o -> o) -> ... -> state -> i -> o
```

`Cons`-specialized:

```haskell
fstream :: (i -> Cons b i) -> (Cons a o -> o) -> ... -> state -> i -> o
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
recursiveEq  :: ... => Mu f -> Mu f -> Bool
recursiveOrd :: ... => Mu f -> Mu f -> Ordering
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
