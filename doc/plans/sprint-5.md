# Sprint 5 — Pretty-printer Derived Combinators

## Scope

Port the standard pretty-printer combinator library: separators
(`hsep`, `vsep`, `sep`), concatenation (`hcat`, `vcat`, `cat`),
indentation (`hang`, `indent`), enclosure (`encloseSep`, `list`,
`tupled`), and filling (`fill`, `fillBreak`).

## Rationale

These are the bread-and-butter combinators users reach for.
Without them, the pretty-printer is technically complete but
impractical. This sprint makes `Data.Fmt.Tree` a usable
pretty-printing API.

## Stories

| ID   | Module / target      | Description                                          |
|------|----------------------|------------------------------------------------------|
| S5.1 | Data.Fmt.Tree        | `concatWith`, `hsep`, `vsep`, `fillSep`              |
| S5.2 | Data.Fmt.Tree        | `hcat`, `vcat`, `fillCat`, `sep`, `cat`              |
| S5.3 | Data.Fmt.Tree        | `hang`, `indent`                                     |
| S5.4 | Data.Fmt.Tree        | `encloseSep`, `list`, `tupled`                       |
| S5.5 | Data.Fmt.Tree        | `fill`, `fillBreak`, `width`                         |
| S5.6 | Data.Fmt.Tree        | `surround`, `punctuate`, `(<+>)`                     |
| S5.7 | Data.Fmt.Tree        | `reAnnotate`, `unAnnotate`, `alterAnnotations`       |
| S5.8 | Test.Prop.Combinators| Properties for all combinators                       |

## New functions

```haskell
-- Separators
(<+>) :: IsString m => Tree m ann -> Tree m ann -> Tree m ann
concatWith :: (Tree m ann -> Tree m ann -> Tree m ann) -> [Tree m ann] -> Tree m ann
hsep, vsep, fillSep :: IsString m => [Tree m ann] -> Tree m ann
sep :: IsString m => [Tree m ann] -> Tree m ann

-- Concatenation
hcat, vcat, fillCat :: [Tree m ann] -> Tree m ann
cat :: [Tree m ann] -> Tree m ann

-- Indentation
hang :: Int -> Tree m ann -> Tree m ann
indent :: IsString m => Int -> Tree m ann -> Tree m ann

-- Enclosure
encloseSep :: Tree m ann -> Tree m ann -> Tree m ann -> [Tree m ann] -> Tree m ann
list, tupled :: IsString m => [Tree m ann] -> Tree m ann

-- Filling
width :: Tree m ann -> (Int -> Tree m ann) -> Tree m ann
fill :: Int -> Tree m ann -> Tree m ann
fillBreak :: Int -> Tree m ann -> Tree m ann

-- Utilities
surround :: Tree m ann -> Tree m ann -> Tree m ann -> Tree m ann
punctuate :: Tree m ann -> [Tree m ann] -> [Tree m ann]

-- Annotations (as folds/hoist)
reAnnotate :: (ann -> ann') -> Tree m ann -> Tree m ann'
unAnnotate :: Tree m ann -> Tree m ann'
alterAnnotations :: (ann -> [ann']) -> Tree m ann -> Tree m ann'
```

## Hedgehog properties (P91–P120)

### Separators (P91–P98)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P91  | `hsep [a,b,c]` = `a <+> b <+> c`                             |
| P92  | `vsep [a,b,c]` puts each on its own line                      |
| P93  | `sep [a,b,c]` = `group (vsep [a,b,c])`                        |
| P94  | `hsep []` = `mempty`                                           |
| P95  | `fillSep` fills as many per line as fit                        |
| P96  | `x <+> y` = `x <> " " <> y` when rendered flat                |
| P97  | `hcat [a,b,c]` = `a <> b <> c`                                |
| P98  | `vcat` = `concatWith (\x y -> x <> hardline <> y)`            |

### Indentation (P99–P102)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P99  | `hang i d` = `align (nest i d)`                               |
| P100 | `indent i d` inserts i spaces then aligns                     |
| P101 | `indent 0 d` = `align d`                                      |
| P102 | `nest i (nest j d)` = `nest (i+j) d`                          |

### Enclosure (P103–P107)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P103 | `list [a,b]` renders as `[a, b]` when fits                    |
| P104 | `list [a,b]` renders with line breaks when doesn't fit        |
| P105 | `tupled [a,b]` renders as `(a, b)` when fits                  |
| P106 | `encloseSep` with custom delimiters works                     |
| P107 | `punctuate "," [a,b,c]` = `[a <> ",", b <> ",", c]`          |

### Filling (P108–P112)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P108 | `fill n d` pads d to width n with spaces                      |
| P109 | `fill n d` does not pad if d exceeds n                         |
| P110 | `fillBreak n d` breaks after d if d exceeds n                  |
| P111 | `width d f` passes rendered width of d to f                    |
| P112 | `surround mid left right` = `left <> mid <> right`             |

### Annotations (P113–P120)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P113 | `reAnnotate id = id`                                           |
| P114 | `reAnnotate (f . g) = reAnnotate f . reAnnotate g`             |
| P115 | `unAnnotate` strips all annotations                            |
| P116 | `unAnnotate . annotate a = id` on rendered output              |
| P117 | `reAnnotate` as `hoist` matches `reAnnotate` as `fold`         |
| P118 | `alterAnnotations pure = id`                                   |
| P119 | Annotation push/pop balance preserved by reAnnotate            |
| P120 | Rendered text content unchanged by reAnnotate                  |

## Work order (TDD)

1. S5.8 — Write P91–P120 skeletons (all red)
2. S5.6 — Implement `(<+>)`, `surround`, `punctuate`, green P96, P112
3. S5.1 — Implement `concatWith`, `hsep`, `vsep`, `fillSep`, green P91–P95
4. S5.2 — Implement `hcat`, `vcat`, `fillCat`, `sep`, `cat`, green P97–P98
5. S5.3 — Implement `hang`, `indent`, green P99–P102
6. S5.4 — Implement `encloseSep`, `list`, `tupled`, green P103–P107
7. S5.5 — Implement `width`, `fill`, `fillBreak`, green P108–P111
8. S5.7 — Implement annotation transforms, green P113–P120
9. Commit when P1–P120 all pass

## Deferred

- `fuse` optimization (Sprint 6 or later)
- `changesUponFlattening` optimization for `group` (Sprint 6)
- Streaming layout via Fix (Cons token) (Sprint 6)
- logfmt refactor to depend on stringfmt (separate project)
- profunctor-optics-strings (separate project)
