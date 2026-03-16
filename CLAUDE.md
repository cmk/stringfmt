# stringfmt

Type-safe formatting as an indexed continuation profunctor,
with pretty-printing via Church-encoded fixed points.

## Project structure

```
src/Data/Fmt.hs           — Core Fmt type, combinators, generic formatters
src/Data/Fmt/Fixed.hs     — Fix type, generic recursion schemes
src/Data/Fmt/Functor.hs   — FmtF pattern functor, Tree type alias, instances
src/Data/Fmt/Tree.hs      — Pretty-printer API: smart constructors, combinators
src/Data/Fmt/String.hs    — ShowS-backed Builder newtype, StringFmt alias
doc/design.md             — Architectural design document
doc/plans/                — Sprint plans (sprint-N.md)
doc/plans/todo.md         — Backlog / future work
doc/notes/                — Dated development notes (notes-YYYY-MM-DD.md)
```

## Development workflow

### Sprint planning

Sprints live in `doc/plans/sprint-N.md` following cirklon conventions:
- Story IDs: `SN.M` (e.g. `S1.1`, `S2.3`)
- Hedgehog properties numbered globally: `P1`, `P2`, ...
- Sections: Scope, Rationale, Stories (table), New types,
  Hedgehog properties (table), Work order (TDD), Deferred,
  Open questions

### TDD work order

1. Write property skeletons (`property $ failure`) — all red
2. Define Hedgehog generators
3. Implement code to green properties one at a time
4. Commit only when all properties pass

### Notes

When asked to "print to notes", append a new section to
`doc/notes/notes-YYYY-MM-DD.md` (create if needed).

## Key design decisions

- `Fix` (Church-encoded, called `Mu` in literature) is parametric
  over any base functor, not specialized to `FmtF`
- No `Recursive`/`Corecursive` typeclasses — explicit schemes only
- Recursion schemes use concrete names: `fold`, `unfold`, `refold`,
  `wrap`, `unwrap`, `hoist`, `foldWithContext`, `foldWithAux`,
  `unfoldShort`
- `FmtF m ann r` is parametric over content type `m`
- Naming: `StringFmt`/`TextFmt`/`ByteFmt` for `Fmt` specializations,
  `Builder` for underlying newtype wrappers

## Dependencies

- `base >= 4.16 && < 5.0`
- `profunctors`
- `bytestring` (planned, Sprint 2)
- `text` (planned, Sprint 2)
- `hedgehog` (test)

## Related projects

- logfmt: existing formatting library (will depend on stringfmt)
- cirklon: Logic/LogicT recursion schemes (shares `Fix`/`Cons` infrastructure)
- profunctor-optics-strings: cotraversal-based Text/ByteString ops (future)
