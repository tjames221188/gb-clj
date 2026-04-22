# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Game Boy (DMG) emulator written in Clojure. The project is in active development — CPU instruction implementations are being added incrementally and validated against Blargg's test ROMs.

## Commands

```bash
# Run the emulator
bb run          # or: clj -M:run

# Run all tests (Kaocha)
bb test         # or: clj -M:test

# Run tests in watch mode (best for iterative development)
bb watch        # or: clj -M:test --watch

# Run a single test namespace
clj -M:test --focus gb-clj.cpu-test

# Start a REPL with dev dependencies
clj -M:dev -m nrepl.cmdline

# Clean caches
bb clean
```

## Architecture

The entire emulator state is a single immutable Clojure map threaded through every operation with `->`. No mutable state — every function takes `state` and returns an updated `state`.

**Top-level state shape:**
```clojure
{:cpu    {...}         ; registers and CPU flags
 :memory [...]        ; flat 65536-element vector (the full address space)
 :serial-output ""}  ; captured serial port output (used by Blargg tests)
```

**CPU state shape** (`initial-state` in `cpu.clj`):
```clojure
{:a :f :b :c :d :e :h :l  ; 8-bit registers
 :pc :sp                   ; 16-bit registers (stored as plain ints)
 :halted? :interrupts-enabled? :t-cycles}
```

### Key namespaces

- **`gb-clj.core`** — entry point, wires together CPU + bus + cart
- **`gb-clj.cpu`** — `step` function: reads opcode at PC, dispatches to `instructions/execute`
- **`gb-clj.cpu.instructions`** — `defmulti execute` dispatched on opcode byte; one `defmethod` per implemented opcode
- **`gb-clj.cpu.prefix-instructions`** — `defmulti execute-prefix` for `0xCB`-prefixed opcodes
- **`gb-clj.cpu.util`** — all CPU helper functions: flag manipulation, 8/16-bit arithmetic, load/store helpers, stack push/pop, rotation primitives
- **`gb-clj.bus`** — memory read/write with address space mapping (ROM guard, echo RAM mirror at `0xE000`, serial output hook at `0xFF02`)
- **`gb-clj.cart`** — ROM file loading and header parsing

### Adding a new opcode

1. Add a `defmethod execute 0xXX MNEMONIC` in `instructions.clj` (or `defmethod execute-prefix` for CB-prefixed opcodes in `prefix_instructions.clj`)
2. Each method must: update registers/memory, call `(util/inc-pc state n)` for the correct byte width, and call `(util/tick state n)` for the correct T-cycle count
3. Flag helpers: `set-flag`, `unset-flag`, `update-flag` with `Z-mask`, `N-mask`, `H-mask`, `C-mask` constants — all in `util.clj`
4. 16-bit register pairs (BC, DE, HL) are stored as two separate 8-bit registers; use `util/get16` / `util/set16` / `util/combine` / `util/split` to work with them

### Testing

Tests use `clojure.test` run by Kaocha. `cpu_test.clj` runs Blargg's `cpu_instrs` test ROMs (`.gb` files in `test-resources/`) by stepping the emulator up to N cycles and checking `:serial-out` for pass/fail strings. Bus boundary behaviour is unit-tested in `bus_test.clj`.

The Blargg test ROM at `test-resources/cpu_instrs/individual/01-special.gb` is the current development target — the `blargg-instr-test` deftest intentionally has a failing assertion (`(is (= 0 1))`) that will be updated as more opcodes are implemented.
