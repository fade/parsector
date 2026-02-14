# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Parsector is a monadic parser combinator library for Common Lisp, designed for user-facing compilers, interpreters, and language readers. It prioritizes human-friendly error reporting (line numbers, columns, error traces) over raw performance. Forked from [Parsnip](https://sr.ht/~shunter/parsnip/) by Samuel Hunter.

**Primary repo**: https://github.com/fade/parsector
**License**: BSD-3-Clause
**Version**: 0.1.0

## Build and Test Commands

```lisp
;; Load the library
(asdf:load-system :parsector)

;; Run all tests
(asdf:test-system :parsector/test/all)
```

CI runs on both SBCL and CCL via GitHub Actions (`.github/workflows.yml`).

## Architecture

### Four-Track Failure Model

Parsector uses continuation-passing style with four continuations per parser:
- **eok** (empty success) - succeeded without consuming input
- **cok** (consumed success) - succeeded with consumed input
- **efail** (empty failure) - failed without consuming input (recoverable)
- **cfail** (consumed failure) - failed after consuming input (hard failure)

This distinction enables better error recovery: most combinators only recover from empty failures. Use `try!` or `handle-rewind` to convert consumed failures to empty failures (requires seekable streams).

### Key Components

**Core data structures** (`parsector.lisp`):
- `parse-stream` - wraps input stream with line/column tracking
- `parser-error` - condition with detailed failure information

**Primary parsers**: `ok`, `fail`, `char-if`, `char-of`, `string-of`, `eof`

**Combinators**:
- Monadic: `flatmap`, `let!`, `handle`, `handle-rewind`
- Sequential: `progn!`, `prog1!`, `prog2!`, `try!`
- Choice: `or!`, `choice`
- Collection: `collect`, `collect1`, `sep`, `sep-by`, `end-by`, `between`, `many-till`
- Chaining: `chainl`, `chainl1`, `chainr`, `chainr1`
- Lookahead: `lookahead`, `not-followed-by`, `optional`

**Convenience parsers**: `digit`, `natural`, `an-integer`, `a-float`, `any-char`, `a-space`, `spaces`, `letter`, `alpha-num`, `hex-digit`, etc.

### Package Structure (package-inferred)

- Main package: `parsector`
- Examples: `parsector/examples/json`, `parsector/examples/tiny-c`, `parsector/examples/m3u`
- Tests: `parsector/test`, `parsector/test/json`, `parsector/test/literals`, `parsector/test/parsec`, `parsector/test/m3u`, `parsector/test/tiny-c`, `parsector/test/all`
- Stats: `parsector/stats/benchmark`, `parsector/stats/coverage`, `parsector/stats/profile`

## Key Files

| File                   | Purpose                                        |
|------------------------|------------------------------------------------|
| `parsector.lisp`       | Core library                                   |
| `parsector.asd`        | ASDF system definition (package-inferred)      |
| `test/package.lisp`    | Main test suite                                |
| `test/json.lisp`       | JSON decoder tests                             |
| `test/literals.lisp`   | Literal parser tests                           |
| `test/parsec.lisp`     | Parsec-compatible combinator tests             |
| `test/m3u.lisp`        | M3U parser tests                               |
| `test/tiny-c.lisp`     | Tiny C parser tests                            |
| `test/all.lisp`        | Loads all test packages                        |
| `examples/json.lisp`   | RFC 8259 JSON decoder                          |
| `examples/tiny-c.lisp` | Minimal C subset parser                        |
| `examples/m3u.lisp`    | M3U playlist parser with expression evaluation |

## Development Patterns

Use `let!` macro for sequencing parsers:
```lisp
(let! ((x parser-a)
       (y parser-b))
  (ok (list x y)))
```

Use `defparser` macro to enable error traces in parser definitions.

Consult `examples/json.lisp` for a complete parser implementation demonstrating idiomatic usage.

## Dependencies

- **Alexandria** (required)
- **Parachute** (testing)
