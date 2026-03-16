# Sprint 3 — Numeric Encoders

## Scope

Port `Data.Fmt.Code` from logfmt to use `Builder` instead of
`LogStr`. Character, string, floating-point, decimal, hex, and
binary encoders.

## Rationale

The Code module is the most used part of logfmt after the core
`Fmt` type. It provides printf-style format specifiers (`d`, `x`,
`f`, `s`, etc.) that need to work with both ByteString.Builder and
Text.Builder.

## Stories

| ID   | Module / target      | Description                                          |
|------|----------------------|------------------------------------------------------|
| S3.1 | Data.Fmt.Code        | Character/string encoders: `c`, `s`, `v`             |
| S3.2 | Data.Fmt.Code        | Floating-point: `e`, `f`, `g` (with precision)       |
| S3.3 | Data.Fmt.Code        | Decimal: `d`, `hhd`, `hd`, `ld`, `lld`              |
| S3.4 | Data.Fmt.Code        | Unsigned: `u`, `hhu`, `hu`, `lu`, `llu`              |
| S3.5 | Data.Fmt.Code        | Hex: `x`, `hhx`, `hx`, `lx`, `llx` (+ fixed-width)  |
| S3.6 | Data.Fmt.Code        | Binary: `b`, `b'`, word-size variants                |
| S3.7 | Test.Prop.Code       | Properties for all encoders                          |

## Hedgehog properties (P51–P70)

| Prop | Description                                                    |
|------|----------------------------------------------------------------|
| P51  | `runByteFmt (c 'A') = "A"`                                    |
| P52  | `runByteFmt (s x) = show x` for Showable x                    |
| P53  | `runByteFmt (d n) = show n` for Int                            |
| P54  | `runByteFmt (u n) = show n` for Word                           |
| P55  | `runByteFmt (x n)` produces lowercase hex                      |
| P56  | `hhx` encodes Word8, `hx` Word16, `lx` Word32, `llx` Word64   |
| P57  | Fixed-width hex pads with zeros to correct width               |
| P58  | `e prec x` matches scientific notation                         |
| P59  | `f prec x` matches fixed-point notation                        |
| P60  | `g prec x` matches general notation (shorter of e/f)           |
| P61  | `d` round-trips: `read (runByteFmt (d n)) = n`                |
| P62  | `u` round-trips: `read (runByteFmt (u n)) = n`                |
| P63  | `x` round-trips: `readHex (runByteFmt (x n)) = n`             |
| P64  | Word-size variants encode in correct byte width                |
| P65  | Binary encoders produce correct byte representations           |
| P66  | `b'` (strict) matches `b` (lazy) content                      |
| P67  | Endianness: `hb` (big-endian) vs `hb'` (little-endian)        |
| P68  | All encoders compose with `%` correctly                        |
| P69  | `"Value: " % d % ", Hex: " % x` smoke test                    |
| P70  | Text equivalents match ByteString equivalents (UTF-8)          |

## Work order (TDD)

1. S3.7 — Write P51–P70 skeletons (all red)
2. S3.1 — Implement character/string encoders, green P51–P52
3. S3.3 — Implement decimal encoders, green P53, P61
4. S3.4 — Implement unsigned encoders, green P54, P62
5. S3.5 — Implement hex encoders, green P55–P57, P63–P64
6. S3.2 — Implement floating-point encoders, green P58–P60
7. S3.6 — Implement binary encoders, green P65–P67
8. Green remaining (P68–P70)
9. Commit when P1–P70 all pass

## Deferred

- Token stream + layout algorithm (Sprint 4)
- ANSI color codes (logfmt concern, not stringfmt)
- Time formatters (logfmt concern, not stringfmt)
