# Sprint 2 — ByteString & Text Modules

## Scope

Add `Data.Fmt.ByteString` and `Data.Fmt.Text` with `ByteFmt`/
`TextFmt` type aliases, runners, and string operations ported
from logfmt (split, replace, hsep, vsep, json/yaml formatting).

## Rationale

These are the two primary output targets. logfmt's existing
`replace1`, `splitWith`, `hsep`, `vsep`, `jsonList`, `yamlList`
need to work with `Builder` instead of `LogStr`. Text gets a
parallel API via `Text.Builder`.

## Dependencies

Add `bytestring` and `text` to `stringfmt.cabal`.

## Stories

| ID   | Module / target         | Description                                         |
|------|-------------------------|-----------------------------------------------------|
| S2.1 | Data.Fmt.ByteString     | `ByteFmt` alias, `runByteFmt`, `printf`             |
| S2.2 | Data.Fmt.ByteString     | `replace1`, `splitWith`, `split1With`               |
| S2.3 | Data.Fmt.ByteString     | `hsep`, `vsep`, `hang`, `list1`, `cat1With`         |
| S2.4 | Data.Fmt.ByteString     | `jsonList`, `yamlList`, `jsonMap`, `yamlMap`         |
| S2.5 | Data.Fmt.Text           | `TextFmt` alias, `runTextFmt`                       |
| S2.6 | Data.Fmt.Text           | Mirror ByteString operations for Text.Builder       |
| S2.7 | Test.Prop.ByteString    | Properties for ByteString operations                |
| S2.8 | Test.Prop.Text          | Properties for Text operations                      |

## New types

```haskell
-- Data.Fmt.ByteString
type ByteFmt = Fmt Builder
runByteFmt :: ByteFmt ByteString a -> a
printf :: ByteFmt (IO ()) a -> a

-- Data.Fmt.Text
type TextFmt = Fmt Text.Builder
runTextFmt :: TextFmt Text a -> a
```

## Hedgehog properties (P31–P50)

### S2.7 — ByteString (P31–P40)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P31  | `runByteFmt (fmt b) = toLazyByteString b`                     |
| P32  | `runByteFmt (fromString s)` encodes s as UTF-8                |
| P33  | `replace1 old new` substitutes first occurrence               |
| P34  | `splitWith` preserves total content                           |
| P35  | `hsep " " [a, b, c] = a <> " " <> b <> " " <> c`            |
| P36  | `vsep [a, b] = a <> "\n" <> b`                                |
| P37  | `jsonList` wraps in brackets with comma separation            |
| P38  | `yamlList` prefixes each with `"- "`                          |
| P39  | `jsonMap` wraps in braces with `": "` between key/value       |
| P40  | `yamlMap` uses `": "` with newline separation                 |

### S2.8 — Text (P41–P50)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P41  | `runTextFmt (fmt b) = toLazyText b`                           |
| P42  | `runTextFmt (fromString s) = pack s`                          |
| P43  | `replace1` for Text mirrors ByteString semantics              |
| P44  | `splitWith` for Text preserves total content                  |
| P45  | `hsep` for Text matches ByteString semantics                  |
| P46  | `vsep` for Text matches ByteString semantics                  |
| P47  | `jsonList` for Text matches ByteString semantics              |
| P48  | `yamlList` for Text matches ByteString semantics              |
| P49  | `jsonMap` for Text matches ByteString semantics               |
| P50  | `yamlMap` for Text matches ByteString semantics               |

## Work order (TDD)

1. S2.1 — Add bytestring/text deps, create ByteString module with ByteFmt + runner
2. S2.7 — Write P31–P40 skeletons (all red)
3. S2.2 — Implement replace1/splitWith, green P33–P34
4. S2.3 — Implement hsep/vsep/hang/list1, green P35–P36
5. S2.4 — Implement json/yaml formatters, green P37–P40
6. S2.5 — Create Text module with TextFmt + runner
7. S2.8 — Write P41–P50 skeletons (all red)
8. S2.6 — Implement Text operations, green P41–P50
9. Commit when P1–P50 all pass

## Deferred

- Numeric encoders (Sprint 3)
- printf ANSI/color support (post-MVP, logfmt concern)
- HTML modules (post-MVP, logfmt concern)
