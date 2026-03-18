# Plan: Refactor logfmt + create profunctor-optics-strings

## Context

logfmt currently depends on fast-logger, ansi-terminal, and time. Most of the core `Fmt` API is generic over any `(Semigroup m, IsString m)` — only the numeric encoders, ByteString splitting, and HTML modules are LogStr-specific. We want to:

1. Shed fast-logger/ansi-terminal/time deps from logfmt's core
2. Add text support
3. Create profunctor-optics-strings: cotraversal-based Text/ByteString ops via Fmt, replacing the MonoTraversable approach in profunctor-optics-sequences
4. Decide whether profunctor-optics-sequences is still needed

## Phase 1: Implement stringfmt MVP

### New module layout

```
Data.Fmt              — Core: Fmt type, combinators, generic formatters (m generic)
Data.Fmt.Code         — Numeric/binary encoders (Builder-based, no fast-logger)
Data.Fmt.ByteString   — ByteString-specific: split, replace, hsep/vsep, json/yaml
Data.Fmt.Text         — Text-specific: same operations but for Text/Text.Builder
Data.Fmt.String       — Endo ShowS
Data.Fmt.Attr         — HTML attributes (stays)
```

### Key changes in Data.Fmt

Replace LogStr with Builder throughout:
- `type LogFmt = Fmt Builder` (was `Fmt LogStr`)
- Remove `ToLogStr` class, use `ToBuilder` or just manual conversion
- `runFmt :: Fmt m m a -> a` (unchanged, generic)
- `runBuilder :: Fmt Builder ByteString a -> a` (replaces `runLogFmt`, uses `toLazyByteString`)
- `runText :: Fmt Text.Builder Text a -> a` (new, uses `toLazyText`)
- `printf :: Fmt Builder (IO ()) a -> a` (replaces `Fmt LogStr Term a`)

### New Data.Fmt.ByteString module

Move ByteString-specific operations from Data.Fmt:
- `replace1`, `splitWith`, `split1With`
- `hsep`, `vsep`, `hang`, `list1`
- `jsonList`, `yamlList`, `jsonMap`, `yamlMap`
- `cat1With`

These now use `Builder` instead of `LogStr`.

### New Data.Fmt.String module

Mirror the ByteString operations for Endo ShowS

### New Data.Fmt.Text module

Mirror the ByteString operations for Text:
- `replace1 :: Text -> Fmt Text.Builder a a -> Fmt Text.Builder a b -> Fmt Text.Builder a b`
- `splitWith :: (Text -> (Text, Text)) -> ...`
- `hsep, vsep, list1, jsonList, yamlList` — Text versions

## Phase 2: Refactor logfmt

### Dropped modules

- `Data.Fmt
- `Data.Fmt.Attr` 
- `Data.Fmt.Html`
- `Data.Fmt.Time

### New modules

Reexports and core API:

- `Data.Fmt.Log

### Dependencies after refactor

```
logfmt: base, stringfmt, fast-logger
```

## Phase 2: Create profunctor-optics-strings

### Concept

Use cotraversals to treat Text/ByteString as higher-kinded containers without MonoTraversable. The `Fmt m` profunctor is `Corepresentable` with `Corep = (->) m`, which is `Distributive`. This means `Fmt m` supports cotraversals natively.

### Module

```
Data.Profunctor.Optic.String    — Cotraversal-based Text/ByteString ops
```

### Core insight: ShortByteString and ShortText are Representable

`ShortByteString` and `ShortText` are backed by `ByteArray#` — fixed-length,
indexable. This makes them `Representable` with `Rep = Int`, hence `Distributive`,
hence they natively support cotraversals. Regular `ByteString`/`Text` go through
Short variants via `toShort`/`fromShort` isos.

### Core API sketch

```haskell
-- ShortByteString is Representable (Rep = Int), hence Distributive
-- This gives us cotraversals for free:
bytes :: Cotraversal' ShortByteString Word8
chars :: Cotraversal' ShortText Char

-- Iso to/from strict variants
short :: Iso' ByteString ShortByteString
shortText :: Iso' Text ShortText

-- Compose: cotraverse through the iso
-- cotraverses (short . bytes) :: (f Word8 -> Word8) -> f ByteString -> ByteString

-- Line/word splitting (these produce variable-length results,
-- so they're Moore machines, not cotraversals)
lined :: Moore ByteString t ByteString b
linedT :: Moore Text t Text b

-- Indexed variants (position-aware)
ibytes :: Cxtraversal' Int ShortByteString Word8
ichars :: Cxtraversal' Int ShortText Char
```

### How it replaces profunctor-optics-sequences

The sequences package has:
- `packing`, `chunking` — encode/decode via IsSequence → replaced by `bytes`/`chars` cotraversals
- `filteredBy`, `takenWhile`, `droppedWhile` — sequence predicates → Moore machines composed with cotraversals
- `partitioned`, `broken`, `spanned` — splitting → `lined`/`worded` + composition
- Pattern synonyms (Lazy/Strict/Chunked) — these remain useful, could move here or stay

### Dependencies

```
profunctor-optics-strings:
  base, bytestring, text, profunctor-optics, logfmt (?)
```

The logfmt dependency is optional — if the Fmt-specific combinators live in logfmt itself, then profunctor-optics-strings just needs profunctor-optics + bytestring + text.

## Phase 3: Decide on profunctor-optics-sequences

After profunctor-optics-strings exists:
- If it covers the useful operations → deprecate/remove profunctor-optics-sequences
- If MonoTraversable-specific operations remain useful (IsSequence, LazySequence, NonNull) → keep sequences as the mono-traversable compat layer
- Pattern synonyms (Lazy/Strict/Chunked/Reversed) could move to profunctor-optics-strings since they only need bytestring/text

## Execution order

1. Refactor logfmt: shed deps, Builder-based, add Text support
2. Build and test logfmt standalone
3. Create profunctor-optics-strings with cotraversal API
4. Evaluate whether profunctor-optics-sequences should be kept

## Verification

- logfmt builds with only base, bytestring, text, profunctors
- Existing formatting examples work with Builder instead of LogStr
- Text.Builder variants produce correct output
- profunctor-optics-strings cotraversals compose with Fmt
- ByteString/Text round-trip through cotraversals

## Future: extract recursion scheme package (data-fix-schemes or similar)

Once stringfmt dust settles (~2-8 sprints), consider splitting the
generic recursion scheme + streaming + Kan infrastructure into an
upstream package. The boundary:

**Upstream (generic, not formatting-specific):**
- Extended scheme zoo on data-fix's Mu/Fix/Nu: foldWithContext,
  foldWithAux, foldGen, unfoldShort, unfoldGen, refoldGen, foldM,
  refoldM, elgot, coelgot, mutu, comutu, prepro, postpro,
  transverse, cotransverse, comap, contramap (Bifunctor)
- Streaming metamorphisms: stream, astream, gstream (generic over
  base functor and fixed-point type)
- Cons pattern functor + fstream, iterate, repeat, toList, fromList,
  all combinators, distributive law specializations
- Pair type + distributive laws (Distribute, lowerAlgebra, etc.)
- Algebra type aliases
- Kan connections: equalDay, compareDay, recursiveEq/Ord (Day for
  structural comparison), YonedaFix (hoist fusion), foldMCodensity,
  codensityToState/stateToCodensity

**Stays in stringfmt (formatting-specific):**
- Fmt type + Cosieve/Corepresentable instances
- Doc pattern functor, Tree type alias
- Pretty-printer API (smart constructors, combinators, layout, render)
- ByteString/Text/String output modules
- Code (numeric encoders)
- fmtDay (Fmt-specific Day connection)

**Why Fmt stays in stringfmt, not upstream:**
Fmt m = Costar ((->) m) — the profunctor instances are trivial
(cosieve = unFmt, cotabulate = Fmt). The value is in the ecosystem
built on top of it, not the type itself. Coindex a b s = Fmt s b a
(profunctor args swapped) lives in profunctor-optics for the colens
carrier story. Both are thin wrappers over Costar ((->) m) and don't
need their own package.

**Why not just depend on recursion-schemes:**
recursion-schemes uses the Recursive/Corecursive typeclass approach.
We commit to Mu everywhere, explicit scheme functions, no typeclass
dispatch. The streaming metamorphisms, Kan connections, and extended
zoo are unique value adds not available in recursion-schemes.
