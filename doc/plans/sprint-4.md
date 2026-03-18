# Sprint 4 — Token Stream & Layout

## Scope

Define the token type and `SimpleDocStream` equivalent as
`Fix (Cons token)`. Implement the layout algorithm as a `refold`
from `Fix (Doc m ann)` to `Fix (Cons (Token m ann))`, plus
rendering from token stream to `Builder`.

## Rationale

This is the core of the pretty-printer: width-sensitive layout.
Without it, `group`/`flatten`/`align` are just tree constructors
with no observable effect. The layout algorithm is the most complex
piece — it threads state (column, nesting) and makes choices at
`Union` nodes based on a fitting predicate.

## Stories

| ID   | Module / target         | Description                                         |
|------|-------------------------|-----------------------------------------------------|
| S4.1 | Data.Fmt.Functor        | `Token m ann` type (SChar, SText, SLine, SAnn*)     |
| S4.2 | Data.Fmt.Tree           | `LayoutOptions`, `PageWidth` types                  |
| S4.3 | Data.Fmt.Tree           | `layoutPretty`: Wadler/Leijen, one-line lookahead   |
| S4.4 | Data.Fmt.Tree           | `layoutCompact`: ignore width, always break         |
| S4.5 | Data.Fmt.Tree           | `render`: fold token stream to Builder              |
| S4.6 | Data.Fmt.Tree           | `pretty`: `layoutPretty` + render, end-to-end       |
| S4.7 | Test.Prop.Layout        | Layout properties                                   |

## New types

```haskell
-- Token: one element of the rendered output stream.
-- This is the base functor element for Fix (Cons (Token m ann)).
data Token m ann
    = TChar !Char
    | TText !Int !m
    | TLine !Int          -- newline + indentation spaces
    | TAnnPush ann
    | TAnnPop

data PageWidth
    = AvailablePerLine !Int !Double   -- columns, ribbon fraction
    | Unbounded

data LayoutOptions = LayoutOptions { layoutPageWidth :: PageWidth }

defaultLayoutOptions :: LayoutOptions

-- Layout: Tree -> token stream (as a list for now, Fix (Cons) later)
layoutPretty :: LayoutOptions -> Tree m ann -> [Token m ann]
layoutCompact :: Tree m ann -> [Token m ann]

-- Render: token stream -> Builder
render :: IsString m => [Token m ann] -> m

-- End-to-end
pretty :: IsString m => LayoutOptions -> Tree m ann -> m
```

## Hedgehog properties (P71–P90)

### Layout correctness (P71–P80)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P71  | `pretty opts (leaf n m) = m` (leaves render as content)        |
| P72  | `pretty opts (x <> y) = pretty opts x <> pretty opts y` (cat) |
| P73  | `pretty opts hardline` contains a newline                      |
| P74  | `pretty opts (nest i (hardline <> x))` indents by i            |
| P75  | `pretty opts (group x)` fits on one line when width allows     |
| P76  | `pretty opts (group x)` breaks when content exceeds width      |
| P77  | `pretty opts emptyDoc = ""`                                    |
| P78  | `pretty opts (flatAlt x y)` uses x (the default)              |
| P79  | `layoutCompact` never uses single-line layout                  |
| P80  | `Fail` in a flattened branch causes fallback to default       |

### Rendering (P81–P85)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P81  | Token stream round-trip: render preserves total content        |
| P82  | `TLine i` renders as newline + i spaces                        |
| P83  | `TAnnPush`/`TAnnPop` are balanced in output                    |
| P84  | Annotations nest correctly (push/pop stack discipline)         |
| P85  | Empty annotations are identity: `annotate a x` renders as `x` when annotations are stripped |

### Integration (P86–P90)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P86  | `align` aligns to current column in rendered output            |
| P87  | Nested `group` picks best layout at each level                 |
| P88  | `pretty (AvailablePerLine 80 1.0)` matches prettyprinter output on simple cases |
| P89  | `pretty Unbounded` always picks single-line layout             |
| P90  | Large trees don't stack overflow (bounded depth stress test)   |

## Work order (TDD)

1. S4.1 — Define Token type
2. S4.7 — Write P71–P90 skeletons (all red)
3. S4.5 — Implement render (fold over token list), green P81–P82
4. S4.4 — Implement layoutCompact, green P71, P73, P77, P79
5. S4.2 — Define LayoutOptions/PageWidth
6. S4.3 — Implement layoutPretty (the hard part), green P74–P76, P78, P80
7. S4.6 — Wire up `pretty`, green remaining P83–P90
8. Commit when P1–P90 all pass

## Open questions

1. Should layout produce `[Token m ann]` (list) or
   `Fix (Cons (Token m ann))` (Church-encoded stream)?
   List is simpler; Fix (Cons) enables streaming metamorphisms
   but adds complexity. Start with list, refactor to Fix later?
2. The fitting predicate: one-line lookahead (prettyprinter's
   `layoutPretty`) vs multi-line (`layoutSmart`). Start with
   one-line only?
3. Trailing whitespace removal: prettyprinter does this as a
   post-processing step on SimpleDocStream. Do we need it?

## Deferred

- `layoutSmart` (multi-line lookahead)
- Trailing whitespace removal
- Fix (Cons) streaming (can retrofit later)
- Streaming metamorphism integration with cirklon
