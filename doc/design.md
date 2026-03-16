# Design: stringfmt

## Core type

```haskell
newtype Fmt m a b = Fmt { unFmt :: (m -> a) -> b }
```

`Fmt m` is `Costar ((->) m)` from `profunctors`, giving `Profunctor`,
`Closed`, `Costrong`, `Cochoice`, `Category`, and `Arrow` instances
for free. The monoid `m` accumulates formatted output, `a` is the
result type, and `b` captures arguments via indexed continuation
passing.

`(->) m` is `Distributive` (representable with `Rep = m`), so
`Costar ((->) m)` is `Corepresentable` with `Corep = (->) m`. This
is what makes `Fmt m` the canonical profunctor for cotraversals and
grates.

## Fmt and Yoneda

```
Fmt m a a  =  (m -> a) -> a        -- for a specific a
forall a. (m -> a) -> a  ≅  m      -- by Yoneda
```

A universally-quantified `Fmt m a a` collapses to `m`. The whole
point of `Fmt` is that it does *not* universally quantify — the
index `b` carries argument structure:

```
Fmt1 m s a     =  Fmt m s (a -> s)          -- one hole
Fmt2 m s a b   =  Fmt m s (a -> b -> s)     -- two holes
```

Testable consequences of Yoneda:

- `runFmt (fmt x) = x` (round-trip)
- `runFmt (f % g) = runFmt f <> runFmt g` (when both are closed)

## Fmt and Day convolution

`(%)` is Day convolution of `(->) m`. Each format hole adds a
Day factor:

```
Day f g a = exists b c. (f b, g c, b -> c -> a)

Day ((->) m) ((->) m) a  ≅  m -> m -> a    -- by co-Yoneda
```

So `Fmt1`, `Fmt2`, `Fmt3` are 1-, 2-, 3-fold Day convolutions.
Day's monoidal laws give free laws for `%`:

- Associativity: `(f % g) % h = f % (g % h)`
- Unit: `fmt mempty % f = f`

The `Curried`/`Day` adjunction (`Day f -| Curried f`) is what makes
the `Closed` instance work. `Curried ((->) m)` is the internal hom
in the Day monoidal category — exactly the structure that
`closed :: p a b -> p (x -> a) (x -> b)` exploits for cotraversals.

### Day convolution and recursion schemes

Day is the "zipping combinator" for recursion schemes. The key
pattern is `lowerDay`/`fold2`:

```haskell
lowerDay phi fta t = phi (Day fta (unwrap t) ($))
fold2 = fold . lowerDay
```

`lowerDay` takes a Day-algebra `phi :: Day f (Base t) b -> b` and
lowers it to a regular algebra by pairing context `f a` with each
unwrapped layer of the structure. `fold2` then folds through the
whole structure with this Day-augmented algebra.

Day-based structural equality and ordering:

```haskell
equalDay eqF (Day f1 f2 fn) =
  eqF (void f1) (void f2)
    && and (zipWith fn (toList f1) (toList f2))

compareDay compareF (Day f1 f2 fn) =
  compareF (void f1) (void f2)
    <> fold (zipWith fn (toList f1) (toList f2))

recursiveEq = fold2 (equalDay (liftEq (==)))
```

`Day Maybe x b` gives a "conditional access" pattern — optionally
take from `x` and combine:

```haskell
-- Conditional take from a stream
takeAnother :: Day Maybe ((,) a) b -> Cons a b
takeAnother = \case
  Day Nothing _ _          -> Nil
  Day (Just x) (h, t) f   -> Cons h (f x t)

takeAvailable :: Day Maybe (Cons a) b -> Cons a b
takeAvailable = \case
  Day Nothing _ _          -> Nil
  Day (Just x) t f         -> fmap (f x) t

-- Conditional access with default
takeNext :: Day Maybe ((,) a) a -> a
takeNext = \case
  Day Nothing (h, _) _    -> h
  Day (Just x) (_, t) f   -> f x t

-- Ordering via Day
le :: Day Maybe Maybe Bool -> Bool
le = \case
  Day Nothing _ _          -> True
  Day (Just a) (Just b) f  -> f a b
  Day (Just _) Nothing _   -> False
```

This connects to `FmtF`: `Day Maybe (FmtF m ann) b` would give
conditional processing of document tree layers (e.g., optional
annotations, conditional nesting).

## Fixed points

```haskell
newtype Fix f = Fix { unFix :: forall a. (f a -> a) -> a }
```

Church-encoded fixed point. Folds are O(1) — the fold *is* the
representation: `fold alg (Fix f) = f alg`.

This is the Church encoding of the least fixed point (called `Mu`
in the recursion-schemes literature). We use `Fix` because it
serves the same role — the representation differs (CPS vs explicit
recursion) but the universal property is the same.

`Fix` is parametric in the base functor `f` — it works with any
`Functor`, not just `FmtF`. This is important because the
pretty-printer needs multiple fixed points, and lazy `ByteString`
and `Text` are themselves fixed points of `Cons`.

No `Recursive`/`Corecursive` typeclass machinery is needed.
Since we commit to `Fix` everywhere, the functor `f` is always
known at the call site and GHC specializes everything.

### Fix and Fmt

`Fix` and `Fmt` share the same functor `(->) m` at their core:

```
Fix ((->) m)  =  forall a. ((m -> a) -> a) -> a  ≅  m   -- by Yoneda
Fmt m a b     =  (m -> a) -> b                            -- indexed, not quantified
```

`Fmt` is `Fix ((->) m)` with the universal quantifier removed and
replaced by indexed continuation passing. Same functor, different
fixed-point strategies. `Fix` takes the fixed point (trees, streams).
`Fmt` keeps it open (indexed continuations, profunctor optics).

### Pattern functors for standard types

Lazy `ByteString` and `Text` are internally `Fix (Cons chunk)`:

```haskell
-- Data.ByteString.Lazy.Internal
data ByteString = Empty | Chunk !StrictByteString ByteString

-- Data.Text.Internal.Lazy
data Text = Empty | Chunk !StrictText Text
```

Their pattern functor is `Cons` specialized to the chunk type:

```
Fix (Cons StrictByteString)  ≅  Lazy.ByteString
Fix (Cons StrictText)        ≅  Lazy.Text
```

Tree-structured containers work the same way:

```haskell
-- Data.IntSet.Internal
data IntSetF r = NilF | TipF Int BitMap | BinF Prefix Mask r r
```

This means the entire pretty-printer pipeline is fold/unfold on
`Fix` with different base functors:

```
Fix (FmtF m ann)                       -- document tree
  → refold → Fix (Cons (Token m ann))  -- token stream
  → fold   → Fix (Cons StrictByteString) -- lazy ByteString
```

And the streaming metamorphisms from cirklon (`stream`, `fstream`,
`astream`) work directly on `Fix (Cons chunk)` since `Cons` is the
same base functor used for `Logic`. So you get incremental
rendering — emit strict chunks as tokens arrive, without
materializing the full stream.

### Recursion schemes

Concrete names, specialized to `Fix`, no typeclass dispatch:

| Name | Type | Use |
|---|---|---|
| `fold` | `(f a -> a) -> Fix f -> a` | All folds (flatten, fuse, reAnnotate, layout) |
| `foldWithContext` | `(f (Fix f, a) -> a) -> Fix f -> a` | group/changesUponFlattening (needs original subtree) |
| `foldWithAux` | `(f b -> b) -> (f (b, a) -> a) -> Fix f -> a` | group (aux: changesUponFlattening, main: build result) |
| `unfold` | `(a -> f a) -> a -> Fix f` | Building trees from seeds |
| `unfoldShort` | `(a -> f (Either (Fix f) a)) -> a -> Fix f` | Unfold with early termination |
| `refold` | `(f b -> b) -> (a -> f a) -> a -> b` | Layout: tree -> stream, fused |
| `hoist` | `(forall a. f a -> g a) -> Fix f -> Fix g` | Strip/transform annotations |

### Kan extension connections

| Kan extension | What it gives us |
|---|---|
| **Yoneda** | `runFmt . fmt = id`, fold fusion law, `hoist` laws |
| **Day** | Laws for `%`, explains `Fmt1`/`Fmt2`/`Fmt3` as iterated convolutions |
| **Curried** | The `Closed` instance / cotraversal structure on `Fmt m` |
| **Ran + Representable** | Indexed decomposition for `ShortByteString`/`ShortText` cotraversals |
| **Codensity** | Efficient monadic tree building (right-associate binds) |

Testable properties:

- `fold alg . wrap = alg . fmap (fold alg)` (fusion law)
- `hoist id = id` (identity)
- `hoist (n . m) = hoist n . hoist m` (composition)
- `wrap . unwrap = id` (Lambek)
- `unwrap . wrap = id` (Lambek)

## Pattern functor: FmtF

```haskell
data FmtF m ann r
    = Fail | Empty | Leaf !Int !m
    | Cat r r | Line | FlatAlt r r
    | Nest !Int r | Union r r | Ann ann r
    | Column (Int -> r) | Nesting (Int -> r)
```

Changes from prettyprinter's `Doc`:

- `Char`/`Text` merged into `Leaf !Int !m` — parametric over
  content type, one fewer constructor
- `WithPageWidth` dropped — rarely used, recoverable via
  `Column`/`Nesting`
- `Fail` retained — needed for lazy failure propagation through
  `Column`/`Nesting` (can't eagerly determine flatten success
  when functions are in the tree)
- Parametric over `m` (content type) — works with `Text`,
  `Builder`, `ShowS`-wrapper, etc.

`Column` and `Nesting` contain functions (`Int -> r`), so
`FmtF` has `Functor` but not `Foldable` or `Traversable`.

`Tree m ann = Fix (FmtF m ann)` is the document tree type, with
`Semigroup` (via `Cat`), `Monoid` (via `Empty`), and `IsString`
(via `Leaf`) instances, making it usable as the monoid in
`Fmt (Tree m ann) a b`.

## Pretty-printer operations as recursion schemes

| Operation | Scheme | Notes |
|---|---|---|
| `flatten` | `fold` | `FlatAlt _ y -> y`, `Line -> Fail` |
| `changesUponFlattening` | `foldWithContext` | Needs original subtree at `Cat` for mixed flat/non-flat children |
| `group` | `foldWithAux` (optimized) | Auxiliary: changesUponFlattening. Main: wrap in `Union` or not |
| `group` (simple) | direct | `union (flatten x) x` — correct, skips optimization |
| `fuse Shallow` | `fold` | Merge adjacent `Leaf` nodes |
| `fuse Deep` | `prepro` | Natural transformation (fuse layer) applied before each step |
| `reAnnotate` | `fold` or `hoist` | Map over annotations |
| `layout` | `refold` | Unfold tree (with stack as state) into token stream |
| Union/fit checking | Elgot algebra | Try one branch, bail to other on `Fail` |
| Incremental layout | streaming metamorphism | Interleave emit/consume via `fstream` pattern |

## Module layout

```
Data.Fmt              — Core: Fmt type, combinators, generic formatters
Data.Fmt.Fixed        — Fix type, generic recursion schemes (fold, unfold, refold, etc.)
Data.Fmt.Functor      — FmtF pattern functor, Tree type alias, instances
Data.Fmt.Tree         — Pretty-printer API: smart constructors, combinators
Data.Fmt.String       — ShowS-backed Builder newtype, StringFmt alias
Data.Fmt.ByteString   — ByteString-specific operations (planned)
Data.Fmt.Text         — Text-specific operations (planned)
Data.Fmt.Code         — Numeric/binary encoders (planned)
Data.Fmt.Attr         — HTML attributes (planned)
```

## Profunctor-optics-strings

The `Closed` instance on `Fmt m` (from `Costar ((->) m)`) gives
native support for cotraversals/grates:

```
Grate s t a b  =  forall p. Closed p => p a b -> p s t

-- Instantiated at Fmt m:
((m -> a) -> b) -> ((m -> s) -> t)
```

`ShortByteString` and `ShortText` are `Representable` with `Rep = Int`
(backed by `ByteArray#`). This gives:

- `Ran ShortByteString Identity a ≅ (Int, a)` — indexed decomposition
  for free, the basis of `ibytes`/`ichars` cotraversals
- Grate laws hold by construction: `tabulate . index = id`

Regular `ByteString`/`Text` go through Short variants via
`toShort`/`fromShort` isos.
