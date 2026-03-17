# Sprint 7 — Doctests, Streaming Layout, Fuse, Group Optimization

## Scope

Four focused tasks that improve quality and performance without
adding new API surface:

1. **Doctest runner** — verify all doc examples in `Data.Fmt.Kan` compile and pass
2. **Streaming layout** — refactor `layoutPretty` to produce `Nu (Cons (Token m ann))` for incremental rendering
3. **`fuse`** — merge adjacent `Leaf` nodes (cata), with optional `prepro` variant for deep fusion
4. **`changesUponFlattening`** — optimize `group` via `foldWithAux` to skip `Union` when doc is already flat or can never flatten

## Stories

| ID   | Module / target      | Description                                          |
|------|----------------------|------------------------------------------------------|
| S7.1 | test/Doctest.hs      | Doctest runner for `Data.Fmt.Kan` examples           |
| S7.2 | Data.Fmt.Tree        | `layoutStream`: produce `Nu (Cons (Token m ann))`    |
| S7.3 | Data.Fmt.Tree        | `renderStream`: fold `Nu` token stream to output     |
| S7.4 | Data.Fmt.Tree        | `prettyStream`: end-to-end streaming pipeline        |
| S7.5 | Data.Fmt.Tree        | `fuse`, `fuseDeep`: merge adjacent `Leaf` nodes      |
| S7.6 | Data.Fmt.Tree        | `changesUponFlattening`, optimized `group'`           |
| S7.7 | Test.Prop.*          | Properties for all new functions                     |

## S7.1 — Doctests

Add `cabal-docspec` or `doctest` test suite that verifies the
examples in `Data.Fmt.Kan`. This is validation that the doc
examples stay correct as the API evolves.

## S7.2–S7.4 — Streaming layout

Current `layoutPretty` produces `[Token m ann]` (a strict list
materialized eagerly). Refactor to produce `Nu (Cons (Token m ann))`
— a seed + step function that lazily generates tokens on demand.

```haskell
-- The layout state is the seed for Nu
data LayoutState m ann = LayoutState
    { lsColumn :: !Int
    , lsPipeline :: [Cmd m ann]
    }

-- Layout as Nu: O(1) construction, tokens generated on demand
layoutStream :: LayoutOptions -> Tree m ann -> Nu (Cons (Token m ann))

-- Render by folding the Nu stream (foldNu)
renderStream :: (Monoid m, IsString m) => Nu (Cons (Token m ann)) -> m

-- End-to-end
prettyStream :: (Monoid m, IsString m) => LayoutOptions -> Tree m ann -> m
```

The existing `layoutPretty`/`render`/`pretty` stay as-is for
backward compat. The streaming variants are strictly better for
large documents.

## S7.5 — Fuse

Merge adjacent `Leaf` nodes to reduce tree size and improve
layout performance. Two variants:

```haskell
-- Shallow: merge adjacent Leaf nodes in Cat chains.
-- Does not recurse into Column/Nesting functions.
fuse :: (Semigroup m) => Tree m ann -> Tree m ann

-- Deep: like fuse but recurses into Column/Nesting via prepro.
fuseDeep :: (Semigroup m) => Tree m ann -> Tree m ann
```

Fuse is a catamorphism that pattern-matches on `Cat (Leaf n1 m1) (Leaf n2 m2)`
and merges them into `Leaf (n1+n2) (m1 <> m2)`. Also merges
`Nest i (Nest j x)` into `Nest (i+j) x`.

## S7.6 — changesUponFlattening

The current `group x = union (flatten x) x` always creates a
`Union` node, even when:
- The doc is already flat (no `FlatAlt`/`Line`) → `Union` is redundant
- The doc can never flatten (contains `Line` outside `FlatAlt`) → `Union` is dead

The optimized version uses `foldWithAux` (zygomorphism):
- Auxiliary algebra: compute `Flatten` result (`Flattened doc' | AlreadyFlat | NeverFlat`)
- Main algebra: only wrap in `Union` when `Flattened`

```haskell
data FlattenResult a = Flattened a | AlreadyFlat | NeverFlat

changesUponFlattening :: Tree m ann -> FlattenResult (Tree m ann)

group' :: Tree m ann -> Tree m ann
group' x = case changesUponFlattening x of
    Flattened x' -> union x' x
    _            -> x
```

## Hedgehog properties (P194–P215)

### Streaming layout (P194–P200)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P194 | `prettyStream opts doc = pretty opts doc` (agreement)          |
| P195 | `layoutStream` produces same tokens as `layoutPretty`          |
| P196 | `renderStream . layoutStream = render . layoutPretty`          |
| P197 | Streaming handles `group` width-sensitivity correctly          |
| P198 | Streaming handles `align` correctly                            |
| P199 | Streaming handles nested `group` correctly                     |
| P200 | Streaming handles annotations correctly                        |

### Fuse (P201–P207)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P201 | `pretty opts (fuse doc) = pretty opts doc` (semantics preserved) |
| P202 | `fuse` merges adjacent leaves                                  |
| P203 | `fuse` merges nested `Nest`                                    |
| P204 | `fuse empty = empty`                                           |
| P205 | `fuse (fuse doc) = fuse doc` (idempotent)                      |
| P206 | `fuseDeep` also preserves semantics                            |
| P207 | `fuseDeep` fuses inside `Column`/`Nesting` functions           |

### Group optimization (P208–P215)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P208 | `pretty opts (group' doc) = pretty opts (group doc)` (agreement) |
| P209 | `changesUponFlattening` returns `AlreadyFlat` for leaves       |
| P210 | `changesUponFlattening` returns `NeverFlat` for `hardline`     |
| P211 | `changesUponFlattening` returns `Flattened` for `line`         |
| P212 | `group'` avoids `Union` for already-flat docs                  |
| P213 | `group'` avoids `Union` for never-flat docs                    |
| P214 | `group'` satisfies Wadler's group laws                         |
| P215 | `group'` agrees with `group` on all Wadler law test cases      |

## Work order (TDD)

1. S7.7 — Write P194–P215 skeletons (all red)
2. S7.1 — Set up doctest runner, verify Kan examples
3. S7.2–S7.4 — Implement streaming layout, green P194–P200
4. S7.5 — Implement fuse/fuseDeep, green P201–P207
5. S7.6 — Implement changesUponFlattening/group', green P208–P215
6. Commit when P1–P215 all pass

## Open questions

1. Should `group'` replace `group` or coexist? If it passes all
   Wadler laws and agrees with `group` on rendered output, it's
   strictly better. But the simple `group` is easier to understand.
2. Should `fuse` be applied automatically during `pretty`/layout?
   prettyprinter applies it optionally via `LayoutOptions`.
3. For streaming layout, should `Union` evaluation be lazy (try
   flat branch lazily, backtrack if doesn't fit) or eager (evaluate
   both, pick)? Lazy is more efficient but harder to implement
   correctly with `Nu`.
