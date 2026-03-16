# Sprint 1 — Foundation Tests

## Scope

Test everything we've built so far: `Fix` recursion scheme laws,
`FmtF`/`Tree` instances, `Fmt` core properties (Yoneda, Day),
`Data.Fmt.String.Builder`. Property skeletons first (all red),
then green them one at a time.

## Rationale

We've built four modules with zero tests. The Yoneda and Lambek
properties are the load-bearing correctness guarantees for everything
downstream. Getting these green now prevents compounding errors in
later sprints.

## Stories

| ID   | Module / target            | Description                                       |
|------|----------------------------|---------------------------------------------------|
| S1.1 | (scaffold)                 | Add `hedgehog` dep, test scaffold, `Test.Prop.*`  |
| S1.2 | Test.Prop.Fixed            | Fix laws: Lambek round-trips, fold fusion, hoist  |
| S1.3 | Test.Prop.Functor          | Tree Semigroup/Monoid/IsString laws               |
| S1.4 | Test.Prop.Tree             | flatten/group/align semantics                     |
| S1.5 | Test.Prop.Fmt              | Fmt Yoneda round-trip, (%) associativity          |
| S1.6 | Test.Prop.String           | Builder Semigroup/Monoid/IsString, runStringFmt   |

## Hedgehog properties

### S1.2 — Fix laws (P1–P8)

| Prop | Description                                               |
|------|-----------------------------------------------------------|
| P1   | `wrap . unwrap = id` (Lambek)                             |
| P2   | `unwrap . wrap = id` (Lambek)                             |
| P3   | `fold alg . wrap = alg . fmap (fold alg)` (fusion)        |
| P4   | `hoist id = id`                                           |
| P5   | `hoist (n . m) = hoist n . hoist m`                       |
| P6   | `refold alg coalg = fold alg . unfold coalg` (coherence)  |
| P7   | `fold wrap = id` (identity fold)                          |
| P8   | `unfold unwrap = id` (identity unfold)                    |

For P1–P8, use a simple test functor (e.g. `Cons Int` or
`FmtF String ()`) with generated trees of bounded depth.

### S1.3 — Tree instances (P9–P14)

| Prop | Description                                               |
|------|-----------------------------------------------------------|
| P9   | `(x <> y) <> z = x <> (y <> z)` (Semigroup assoc)        |
| P10  | `mempty <> x = x` (Monoid left identity)                  |
| P11  | `x <> mempty = x` (Monoid right identity)                 |
| P12  | `fromString s <> fromString t = fromString (s ++ t)` (IsString homomorphism) |
| P13  | `fromString "" = mempty`                                  |
| P14  | Generated tree round-trips through `unwrap . wrap`        |

For P9–P14, test at `Tree String ()`. Need a tree generator
that builds bounded-depth trees using smart constructors.

### S1.4 — Tree combinators (P15–P20)

| Prop | Description                                               |
|------|-----------------------------------------------------------|
| P15  | `flatten hardline = fail_` (hardline can't flatten)       |
| P16  | `flatten (flatAlt x y) = flatten y` (uses flat branch)    |
| P17  | `flatten (union x y) = flatten x` (takes flatter branch)  |
| P18  | `flatten (leaf n m) = leaf n m` (leaves unchanged)        |
| P19  | `group x = union (flatten x) x` (definition)             |
| P20  | `align` column/nesting interaction (at least one property)|

### S1.5 — Fmt core (P21–P26)

| Prop | Description                                               |
|------|-----------------------------------------------------------|
| P21  | `runFmt (fmt x) = x` (Yoneda round-trip)                 |
| P22  | `runFmt (f % g) = runFmt f <> runFmt g` (% homomorphism) |
| P23  | `(f % g) % h = f % (g % h)` (% associativity)            |
| P24  | `fmt mempty % f = f` (% left unit)                        |
| P25  | `f % fmt mempty = f` (% right unit)                       |
| P26  | `runFmt (fmt1 show % " = " % fmt1 show) 42 "hello" = "42 = hello"` (smoke test) |

### S1.6 — String Builder (P27–P30)

| Prop | Description                                               |
|------|-----------------------------------------------------------|
| P27  | `runStringFmt (fromString s) = s` (IsString round-trip)   |
| P28  | `runStringFmt (x <> y) = runStringFmt x ++ runStringFmt y`|
| P29  | `runStringFmt mempty = ""`                                |
| P30  | `unBuilder (fromString s) = (s ++)` (ShowS semantics)     |

## Work order (TDD)

1. S1.1 — Scaffold: cabal test-suite, hedgehog dep, empty test modules
2. S1.2 — Write P1–P8 skeletons (`property $ failure`), then green them
3. S1.3 — Write P9–P14 skeletons, implement tree generator, green them
4. S1.4 — Write P15–P20 skeletons, green them
5. S1.5 — Write P21–P26 skeletons, green them
6. S1.6 — Write P27–P30 skeletons, green them
7. Commit when P1–P30 all pass

## Deferred

- ByteString/Text modules (Sprint 2)
- Numeric encoders (Sprint 3)
- Token stream + layout algorithm (Sprint 4)
- Pretty-printer derived combinators (Sprint 5)

## Open questions

1. Tree generator depth bound: what's a reasonable max depth for
   property testing? (5? 10? Need to balance coverage vs speed)
2. Testing `Column`/`Nesting` — these contain functions, so can't
   test structural equality directly. Test via layout/rendering?
3. Should P12 (IsString homomorphism) hold for Tree? `fromString`
   caches length per leaf, so `fromString (s ++ t)` is one leaf
   while `fromString s <> fromString t` is `Cat` of two leaves.
   These are semantically equal but structurally different —
   need to define equality via a rendering fold.
