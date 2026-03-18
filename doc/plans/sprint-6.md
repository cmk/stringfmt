# Sprint 6 — Kan Extensions: Day, Yoneda, Codensity

## Scope

Add `kan-extensions` dependency and create `Data.Fmt.Kan` module
that connects `Fmt`, `Fix`, and the Kan extension types with
documented examples and properties. Focus on the three most
practically useful types: Day convolution, Yoneda, and Codensity.

## Rationale

These connections are the theoretical backbone of the library
(see design.md) but currently exist only in documentation. Making
them concrete with code, doctests, and properties:

- **Day**: explains `(%)` as Day convolution of `(->) m`, gives
  laws for `Fmt1`/`Fmt2`/`Fmt3`, enables `fold2`/`cata2` pattern
  for parallel folds (structural equality, zipping)
- **Yoneda**: `runFmt . fmt = id` and fusion laws; `Yoneda f`
  fuses `fmap` chains; connects `Fix` and `Fmt` via the
  Yoneda lemma
- **Codensity**: right-associates monadic binds for efficient
  tree building; `Codensity ((->) m)` is `State m`; improves
  `foldM` performance on deep trees
- **Ran/Lan**: right/left Kan extensions; `Ran` + `Representable`
  gives indexed decomposition for `ShortByteString`/`ShortText`
  cotraversals; `Ran g h` is the "best approximation" of `h`
  through `g`
- **Density**: comonad from `Lan f f`; dual of Codensity;
  `Density ((->) m)` is trivial (collapses to identity) but
  useful for other functors
- **Curried**: right adjoint to Day (`Day f -| Curried f`);
  this is what makes `Fmt m`'s `Closed` instance work —
  `Curried ((->) m)` is the internal hom in the Day monoidal
  category

## Stories

| ID   | Module / target      | Description                                          |
|------|----------------------|------------------------------------------------------|
| S6.1 | stringfmt.cabal      | Add `kan-extensions` dependency                      |
| S6.2 | Data.Fmt.Kan         | Day convolution: `lowerDay`, `fold2`, `equalDay`, `compareDay` |
| S6.3 | Data.Fmt.Kan         | Day + Fmt: `(%)` as Day, `Fmt1`/`Fmt2` as iterated Day |
| S6.4 | Data.Fmt.Kan         | Yoneda: `liftYoneda`/`lowerYoneda` for `Fix`, fmap fusion |
| S6.5 | Data.Fmt.Kan         | Codensity: `lowerCodensity` for `foldM`, build/fold fusion |
| S6.6 | Data.Fmt.Kan         | Ran/Lan: `Ran g h` for indexed decomposition, `gran`/`glan` |
| S6.7 | Data.Fmt.Kan         | Density: `Lan f f` comonad, connection to cotraversals |
| S6.8 | Data.Fmt.Kan         | Curried: `Day f -| Curried f` adjunction, `Closed` connection |
| S6.9 | Data.Fmt.Kan         | Documented examples (doctests) for all combinators   |
| S6.10 | Test.Prop.Kan       | Hedgehog properties                                  |

## New types and functions

```haskell
-- Day convolution applied to recursion schemes
--
-- lowerDay pairs context f a with one unwrapped layer,
-- enabling two-structure folds:
--
-- >>> let xs = fromList [1,2,3 :: Int]
-- >>> let ys = fromList [10,20,30 :: Int]
-- >>> fold2 (\(Day cx cy combine) -> elim 0 (\_ r -> combine cx cy + r)) xs ys
-- -- pointwise sum via Day
lowerDay
    :: Functor f
    => (Day f g b -> b)       -- ^ Day-algebra
    -> f a                    -- ^ context
    -> Fix g                  -- ^ structure to fold
    -> b

fold2
    :: Functor f
    => (Day f g b -> b)       -- ^ Day-algebra
    -> Fix f                  -- ^ first structure
    -> Fix g                  -- ^ second structure (folded)
    -> b

-- Structural equality via Day (the Eq1 connection):
--
-- >>> equalDay (liftEq (==)) (Day (Cons 1 True) (Cons 1 True) (&&))
-- True
equalDay
    :: (Foldable f, Eq1 f)
    => (f () -> f () -> Bool) -- ^ shape equality
    -> Day f f Bool           -- ^ paired layer with combining function
    -> Bool

-- Structural ordering via Day:
compareDay
    :: (Foldable f, Ord1 f)
    => (f () -> f () -> Ordering)
    -> Day f f Ordering
    -> Ordering

-- Recursive equality/ordering via fold2 + Day:
--
-- >>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,3])
-- True
-- >>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,4])
-- False
recursiveEq :: (Functor f, Foldable f, Eq1 f) => Fix f -> Fix f -> Bool
recursiveOrd :: (Functor f, Foldable f, Ord1 f) => Fix f -> Fix f -> Ordering

-- Yoneda for Fix: fuse chains of hoist (natural transformations)
--
-- Multiple hoists without Yoneda:
--   hoist n3 . hoist n2 . hoist n1  -- 3 traversals
--
-- With Yoneda:
--   lowerYoneda . fmap n3 . fmap n2 . fmap n1 . liftYoneda  -- 1 traversal
--
-- >>> let xs = fromList [1,2,3 :: Int]
-- >>> lowerYoneda (fmap (\(Cons a r) -> Cons (a+1) r) (liftYoneda xs))
-- -- equivalent to hoist (\(Cons a r) -> Cons (a+1) r) xs
liftFix :: Functor f => Fix f -> Yoneda (Fix' f)
lowerFix :: Functor f => Yoneda (Fix' f) -> Fix f

-- Codensity for efficient monadic folds
--
-- foldM on deep left-nested trees can be O(n^2) due to
-- left-associated (>>=). Codensity right-associates:
--
-- >>> import Control.Monad.Codensity
-- >>> let xs = fromList [1..1000 :: Int]
-- >>> lowerCodensity (foldM (\(Cons a b) -> Codensity $ \k -> k (a + b)) xs)
-- -- O(n) instead of O(n^2)
foldMCodensity
    :: (Traversable f, Monad m)
    => (f a -> Codensity m a)
    -> Fix f
    -> m a

-- Day and Fmt: showing (%) is Day convolution
--
-- @
-- (%) :: Semigroup m => Fmt m b c -> Fmt m a b -> Fmt m a c
-- @
--
-- This is Day convolution of (->) m:
--
-- @
-- Day ((->) m) ((->) m) a  ≅  m -> m -> a   (by co-Yoneda)
-- @
--
-- Each format hole adds a Day factor:
--
-- @
-- Fmt1 m s a   ~ 1-fold Day of (->) m
-- Fmt2 m s a b ~ 2-fold Day of (->) m
-- @
fmtDay
    :: Semigroup m
    => Fmt m b c              -- ^ left formatter
    -> Fmt m a b              -- ^ right formatter
    -> Day ((->) m) ((->) m) c -- ^ Day representation

-- Ran: right Kan extension
--
-- Ran g h a = forall b. (a -> g b) -> h b
--
-- For Representable u: Ran u Identity a ≅ (Rep u, a)
-- This is the indexed decomposition used by profunctor-optics-strings.
--
-- >>> import Data.Functor.Kan.Ran
-- >>> gran (toRan id (Just 42)) == Just 42
-- True
ranToIndex
    :: Representable u
    => Ran u Identity a
    -> (Rep u, a)

-- Lan: left Kan extension (dual of Ran)
--
-- Lan g h a = exists b. (g b -> a, h b)
--
-- Lan Identity f ≅ Coyoneda f (free functor)
--
-- >>> import Data.Functor.Kan.Lan
-- >>> toLan id (glan (Just 42)) == Just 42
-- True

-- Density: comonad from Lan f f
--
-- Density k a = exists b. (k b -> a, k b)
--
-- The comonadic dual of Codensity. extract applies
-- the function to the value:
--
-- >>> import Control.Comonad.Density
-- >>> extract (liftDensity (Identity 42))
-- 42

-- Curried: right adjoint to Day convolution
--
-- Curried g h a = forall r. g (a -> r) -> h r
--
-- Day f -| Curried f. This adjunction is what makes
-- Fmt m's Closed instance work. The internal hom in
-- the Day monoidal category is Curried.
--
-- >>> import Data.Functor.Day.Curried
-- >>> lowerCurried (liftCurried (Identity 42)) == Identity 42
-- True
```

## Hedgehog properties (P141–P160)

### Day convolution (P141–P148)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P141 | `equalDay` agrees with `(==)` for `Cons Int`                  |
| P142 | `compareDay` agrees with `compare` for `Cons Int`              |
| P143 | `recursiveEq` agrees with `(==)` on `Fix (Cons Int)`          |
| P144 | `recursiveOrd` agrees with `compare` on `Fix (Cons Int)`       |
| P145 | `fold2` sum: pointwise sum of two lists via Day                |
| P146 | `fold2` zip: zip two lists via Day                             |
| P147 | `lowerDay` with identity context = `fold`                      |
| P148 | Day associativity: `(f % g) % h` agrees with `f % (g % h)` via Day |

### Yoneda (P149–P153)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P149 | `lowerYoneda . liftYoneda = id` (round-trip)                   |
| P150 | Yoneda fuses multiple `fmap`: single traversal                 |
| P151 | `runFmt (fmt x) = x` (Yoneda for Fmt, already P21 but via Yoneda types) |
| P152 | Yoneda `fmap` fusion: `fmap f . fmap g = fmap (f . g)` holds automatically |
| P153 | `liftYoneda` preserves structure (toList before = toList after) |

### Codensity (P154–P158)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P154 | `foldMCodensity` agrees with `foldM` on small trees            |
| P155 | `lowerCodensity . pure = pure` (identity)                      |
| P156 | Codensity `>>=` is right-associated (structural, not just semantic) |
| P157 | `Codensity ((->) m)` ≅ `State m` (isomorphism)                 |
| P158 | `foldMCodensity` handles deep trees without stack overflow      |

### Day + Fmt integration (P159–P160)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P159 | `fmtDay` round-trips: Day representation agrees with `%`       |
| P160 | Day unit: `Day ((->) m) Identity` is the identity for `%`      |

### Ran / Lan (P161–P165)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P161 | `gran . toRan = id` (Ran round-trip)                           |
| P162 | `Ran Identity f ≅ Yoneda f` (Ran specialization)               |
| P163 | `glan . fromLan = id` (Lan round-trip)                         |
| P164 | `Lan Identity f ≅ Coyoneda f` (Lan specialization)             |
| P165 | `ranToRep` gives `(Rep u, a)` for Representable `u`            |

### Density / Curried (P166–P170)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P166 | `Density f` extract/duplicate laws (comonad)                   |
| P167 | `densityToLan . lanToDensity = id` (round-trip)                |
| P168 | `Curried f g` adjunction: `applied . unapplied = id`           |
| P169 | `liftCurried`/`lowerCurried` round-trip for Applicative f      |
| P170 | `Curried ((->) m) Identity ≅ (m, -)` (the internal hom)        |

## Work order (TDD)

1. S6.1 — Add `kan-extensions` to cabal deps
2. S6.7 — Write P141–P160 skeletons (all red)
3. S6.2 — Implement Day combinators, green P141–P148
4. S6.4 — Implement Yoneda combinators, green P149–P153
5. S6.5 — Implement Codensity combinators, green P154–P158
6. S6.3 — Implement Day+Fmt bridge, green P159–P160
7. S6.6 — Implement Ran/Lan combinators, green P161–P165
8. S6.7 — Implement Density combinators, green P166–P167
9. S6.8 — Implement Curried combinators, green P168–P170
10. S6.9 — Add doctests to all exported functions
11. Commit when P1–P170 all pass

## Documentation requirements

Every exported function in `Data.Fmt.Kan` must have:
1. A type signature with named arguments
2. A Haddock description explaining what it does in plain English
3. At least one doctest showing concrete usage
4. A "connection" comment explaining the categorical significance

Example:
```haskell
-- | Fold two structures in parallel using Day convolution.
--
-- @fold2 alg xs ys@ unwraps one layer from @ys@, pairs it
-- with the corresponding layer from @xs@ via 'Day', and
-- applies @alg@ to produce the result. This is how structural
-- equality works: each layer is paired and compared.
--
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> let ys = fromList [10, 20, 30 :: Int]
-- >>> fold2 (\(Day (Cons a1 r1) (Cons a2 r2) k) -> a1 + a2 + k r1 r2) xs ys
-- 66
--
-- __Connection:__ @fold2@ is @fold . lowerDay@ — it lowers a
-- Day-algebra to a regular algebra by pairing with 'unwrap'.
fold2 :: ...
```

## Open questions

1. Should `Data.Fmt.Kan` re-export the relevant types from
   `kan-extensions`, or should users import both?
2. The Yoneda-for-Fix connection needs a newtype wrapper
   (Yoneda works on `* -> *` but `Fix` is `(* -> *) -> *`).
   Use `Yoneda (Fix' f)` where `newtype Fix' f a = Fix' (Fix f)`?
   Or skip the Yoneda-for-Fix and just document the principle?
3. `Codensity ((->) m) ≅ State m` — worth providing explicit
   iso functions, or just document it?

## Deferred

- Day convolution for `Doc` specifically (needs careful thought
  about function-valued constructors `Column`/`Nesting` — these
  block `Foldable`, so `equalDay`/`compareDay` won't work for
  `Tree` directly)
- `Ran` + `Representable` for `ShortByteString`/`ShortText`
  cotraversals (profunctor-optics-strings, separate project)
- `Coyoneda` for free functor construction (less immediately
  useful than Yoneda for our purposes)
