# Implementation Progress

Last updated: 2026-08-20

## Unprefixed Opcodes

`✓` = implemented, `·` = missing (not yet needed or not yet written), `✗` = invalid opcode (hard-locks real hardware, safe to leave unimplemented)

```
     0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
0x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
1x   ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
2x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
3x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
4x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
5x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
6x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
7x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
8x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
9x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Ax   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Bx   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Cx   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Dx   ✓    ✓    ✓    ✗    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✗    ✓    ✗    ✓    ✓
Ex   ✓    ✓    ✓    ✗    ✗    ✓    ✓    ✓    ✓    ✓    ✓    ✗    ✗    ✗    ✓    ✓
Fx   ✓    ✓    ✓    ✓    ✗    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✗    ✗    ✓    ✓
```

**Implemented: 244 / 245 valid opcodes (99.6%)** — only `0x10 STOP` remains.

### Notable missing unprefixed opcodes

| Opcode | Mnemonic | Notes |
|--------|----------|-------|
| `0x10` | `STOP` | Low priority |

---

## CB-Prefixed Opcodes

```
     0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
CB0x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB1x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB2x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB3x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB4x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB5x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB6x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB7x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB8x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB9x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CBAx ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CBBx ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CBCx ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CBDx ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CBEx ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CBFx ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
```

**Implemented: 256 / 256 CB opcodes (100%)**

All CB-prefixed opcodes implemented.

---

## Peripherals

- **Timer implemented** (`timer.clj`) — DIV (0xFF04), TIMA (0xFF05), TMA (0xFF06), TAC (0xFF07). DIV is the upper byte of a free-running 16-bit `:div-counter`, reset on any write. TIMA accumulates T-cycles in `:tima-counter` per TAC's clock-select divisor, overflows reload from TMA and set IF bit 2. Wired into `cpu/step`, which now measures T-cycles elapsed per step and calls `timer/tick` with the delta.
- No LCD/PPU — Blargg tests may still need vblank timing for later sub-tests

## Known Issues / TODOs

- **Memory access timing is not cycle-accurate** — `cpu/step` executes a whole instruction
  against a frozen snapshot then flushes its T-cycles to the timer afterwards, so every
  memory access in an instruction appears to happen at the instruction boundary. Totals are
  correct (`instr_timing` passes); the *distribution* within each instruction is not. This
  fails all three `mem_timing` ROMs. **See [PLAN-mem-timing.md](PLAN-mem-timing.md)** for
  the diagnosis, the derived M-cycle rule, and a step-by-step fix.
- Halt bug (`0x76` + `do-halt-bug`) is an infinite loop — `halt_bug.gb` will hang. See plan.
- Echo RAM (0xE000–0xFDFF) write mapping is implemented but read mapping is not

## Architecture notes

- `gb-clj.cpu.bits` holds `combine`/`split` (pure 8/16-bit byte helpers) with zero dependencies, so both `bus.clj` and `cpu/util.clj` can use them without a circular require. This pattern (dependency-free namespace for anything both `bus` and `cpu/util` need) is the way to resolve future cycles of the same shape — see how `timer.clj` also avoids requiring `bus.clj` for the same reason.
- `util/inc16` and `util/dec-r16` both have a single-register arity (`[gb-state r]`) for SP alongside the two-register arity for register pairs like BC/DE/HL — follow that pattern for any future SP-specific 16-bit helper.
- `util/maybe-call` (predicate-parameterized, mirrors `util/maybe-ret`/`jump-relative-pred-*`) backs all four conditional `CALL` opcodes (`0xC4`/`0xCC`/`0xD4`/`0xDC`) — each defmethod is a one-liner passing a different flag predicate.
- `util/add-val` (no carry-in) delegates to `util/add-with-carry` with the carry flag forced off, mirroring `sub-val`'s delegation to `sub-with-carry` — used by the `0x80–0x87 ADD A,r` family.
- `util/toggle-flag` (bit-xor, mirrors `set-flag`'s bit-or / `unset-flag`'s bit-and-not) flips a flag bit without reading it first — used by `0x3F CCF`.
- `util/rst` backs all 8 RST vectors (`0xC7/CF/D7/DF/E7/EF/F7/FF`) — unconditional push-and-jump to a fixed address, same shape as `maybe-call`'s taken branch but with `PC+1` (not `PC+3`) as the return address since RST has no operand bytes.
- `util/half-carry?` (bit-4 XOR trick) is only valid for genuine **two-operand** arithmetic (`result = a + b` or `a - b`, no separate carry-in) — used correctly by `sub-val` and the plain `0xC6 ADD_A_N`/`0xD6 SUB_A_N`. **Do not** fold a carry-in into `b` via `(+ val old-c)` and pass that to `half-carry?` — it looks like it should work but silently breaks when `val + old-c` itself ripple-carries across a nibble boundary independent of `a` (concrete counterexample: `a=0, val=0x0F, old-c=1` — folded-carry version gives H=false, correct answer is true). This caused a real bug that broke `04-op r,imm.gb` (ADC/SBC) until caught by a brute-force check over all 256×256×2 input combos. `add-with-carry` and `sub-with-carry` (the WITH-CARRY variants) must use the direct nibble-sum/nibble-borrow comparison instead: `(> (+ (bit-and a 0xF) (bit-and val 0xF) old-c) 0xF)` for add, `(< (bit-and a 0xF) (+ (bit-and val 0xF) old-c))` for subtract. `sub-val` (no carry-in) delegates to `sub-with-carry` with the carry flag forced off — same delegation approach is the natural template for `add-val` when the `0x80–0x87 ADD A,r` family gets built, but remember it inherits the *nibble-comparison* H flag, not `half-carry?`.
- `util/and-val` mirrors `or-val`/`xor-val` (same shape: `Z` from result, `N` cleared, `C` cleared) with one quirk — `AND` always **sets** `H`, unlike `OR`/`XOR` which clear it. Backs the `0xA0–0xA7 AND r` family and `0xE6 AND_N`.
- **Bucketed multimethod dispatch** — `execute-prefix`'s dispatch function is a `cond`, not a bare identity: exact opcodes still dispatch on themselves (the existing per-opcode `defmethod`s), but whole ranges (`0x40–0x7F` BIT, `0x80–0xBF` RES, `0xC0–0xFF` SET — 192 opcodes total) bucket to a single keyword each and are handled by one `defmethod` per family, decoding the *actual* opcode (now passed through instead of ignored as `_`) via the shared `parse-opcode` helper into a bit-index (`(bit-and (bit-shift-right opcode 3) 0x07)`) and register (`(bit-and opcode 0x07)`, mapped via a `[:b :c :d :e :h :l :hl :a]` vector). Avoids 192 hand-written one-liners. `derive`/`isa?` hierarchies were considered and rejected — they only encode discrete "is-a" relationships you declare one edge at a time, not numeric ranges, so they'd need the same per-opcode declarations a `cond` avoids in one line.
- `RES`/`SET` share a `bit-update` helper parameterized by `bit-fn` (`bit-clear`/`bit-set`) — both leave all flags untouched (unlike `BIT`, which sets `Z`/`H`), and both are full read-modify-write for `(HL)` (+8 cycles, 16 total) rather than `BIT`'s read-only `(HL)` (+4 cycles, 12 total).
- `util/maybe-ret`'s "taken" branch is specifically for *conditional* `RET` (20 cycles) — unconditional returns (`0xC9 RET`, `0xD9 RETI`) must NOT delegate to it, since they tick 16, not 20. Both are hand-written with the same pop-and-jump shape instead.
- `cpu_test.clj`'s `get-test-rom`/`run-test-rom` take a full resource-relative path now (e.g. `"cpu_instrs/individual/01-special.gb"`), not a bare filename under an assumed `cpu_instrs/individual/` prefix — needed once test ROMs outside `cpu_instrs` (e.g. `instr_timing/instr_timing.gb`) got wired in.

## Test Status

- `01-special.gb` — **Passed** ✓
- `02-interrupts.gb` — **Passed** ✓ — was hanging in a `HALT` loop waiting on the timer interrupt; fixed by having `halty-walty`'s idle branch (nothing pending) call `(util/tick gb-state 4)` instead of returning state untouched. Previously, `HALT` froze `:cpu :t-cycles`, which froze `timer/tick`'s elapsed-cycle delta, which froze DIV/TIMA — so the timer interrupt the CPU was halted waiting for could never fire. Pre-existing bug in `cpu.clj`, only surfaced once TIMA existed to be waited on.
- `03-op sp,hl.gb` — **Passed** ✓
- `04-op r,imm.gb` — **Passed** ✓
- `05-op rp.gb` — **Passed** ✓
- `06-ld r,r.gb` — **Passed** ✓
- `07-jr,jp,call,ret,rst.gb` — **Passed** ✓
- `08-misc instrs.gb` — **Passed** ✓
- `09-op r,r.gb` — **Passed** ✓
- `10-bit ops.gb` — **Passed** ✓
- `11-op a,(hl).gb` — **Passed** ✓
- `mem_timing/01-read_timing.gb` — **Failed** ✗ — `F0:2-3 FA:2-4 CB 46..7E:2-3`
- `mem_timing/02-write_timing.gb` — **Failed** ✗ — `36:2-3 E0:2-3 EA:2-4`
- `mem_timing/03-modify_timing.gb` — **Failed** ✗ — `34/35:0/0-2/3`, all CB `(HL)` RMW `0/0-3/4`
- `instr_timing.gb` — **Passed** ✓ — caught a real cycle-count bug: `0xD9 RETI` delegated to `util/maybe-ret` with `(constantly true)`, inheriting the conditional-RET-taken tick count (20) instead of the unconditional tick count (16) that `RETI` (like plain `0xC9 RET`) actually needs. Fixed by giving `RETI` the same hand-written pop-and-jump shape as `RET`, plus setting IME.
