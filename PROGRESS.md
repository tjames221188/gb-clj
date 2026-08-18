# Implementation Progress

Last updated: 2026-08-18

## Unprefixed Opcodes

`✓` = implemented, `·` = missing (not yet needed or not yet written), `✗` = invalid opcode (hard-locks real hardware, safe to leave unimplemented)

```
     0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
0x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
1x   ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
2x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·
3x   ✓    ✓    ✓    ✓    ·    ✓    ✓    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·
4x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
5x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
6x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
7x   ✓    ✓    ✓    ✓    ✓    ✓    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·
8x   ·    ·    ·    ·    ·    ·    ·    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
9x   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
Ax   ·    ·    ·    ·    ·    ·    ·    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Bx   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Cx   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·    ✓    ✓    ✓    ✓    ·    ✓    ✓    ·
Dx   ✓    ✓    ✓    ✗    ·    ✓    ✓    ·    ✓    ·    ✓    ✗    ·    ✗    ·    ·
Ex   ✓    ✓    ✓    ✗    ✗    ✓    ✓    ·    ✓    ✓    ✓    ✗    ✗    ✗    ✓    ·
Fx   ✓    ✓    ✓    ✓    ✗    ✓    ✓    ·    ·    ·    ✓    ✓    ✗    ✗    ✓    ✓
```

**Implemented: 167 / 245 valid opcodes (68%)**

### Notable missing unprefixed opcodes

| Opcode | Mnemonic | Notes |
|--------|----------|-------|
| `0x10` | `STOP` | Low priority |
| `0x2F` | `CPL` | Complement A — next up after DAA |
| `0x34` | `INC (HL)` | |
| `0x37` | `SCF` | Set carry flag |
| `0x3F` | `CCF` | Complement carry flag |
| `0x76` | `HALT` | Needs interrupt system |
| `0x7F` | `LD A,A` | Trivial no-op load |
| `0x80–0x87` | `ADD A,r` | Entire ADD family missing |
| `0x90–0x9F` | `SUB/SBC r` | Entire SUB/SBC families missing |
| `0xA0–0xA7` | `AND r` | Entire AND family missing |
| `0xCC` | `CALL Z,a16` | |
| `0xD4` | `CALL NC,a16` | |
| `0xDC` | `CALL C,a16` | |
| RST family | `0xC7/CF/D7/DF/E7/EF/F7/FF` | All 8 RST vectors missing |

---

## CB-Prefixed Opcodes

```
     0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
CB0x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB1x ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
CB2x ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CB3x ·    ·    ·    ·    ·    ·    ·    ·    ✓    ·    ·    ·    ·    ·    ·    ·
CB4x ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CB5x ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CB6x ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CB7x ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CB8x ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CB9x ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CBAx ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CBBx ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CBCx ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CBDx ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CBEx ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
CBFx ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
```

**Implemented: 33 / 256 CB opcodes (13%)**

Missing entire CB groups: SLA (CB2x), SRA (CB2x), SWAP (CB3x, except 0x38 SRL_B), SRL (CB3x), BIT (CB4x–CB7x), RES (CB8x–CBBx), SET (CBCx–CBFx)

---

## Peripherals

- **Timer implemented** (`timer.clj`) — DIV (0xFF04), TIMA (0xFF05), TMA (0xFF06), TAC (0xFF07). DIV is the upper byte of a free-running 16-bit `:div-counter`, reset on any write. TIMA accumulates T-cycles in `:tima-counter` per TAC's clock-select divisor, overflows reload from TMA and set IF bit 2. Wired into `cpu/step`, which now measures T-cycles elapsed per step and calls `timer/tick` with the delta.
- No LCD/PPU — Blargg tests may still need vblank timing for later sub-tests

## Known Issues / TODOs

- Echo RAM (0xE000–0xFDFF) write mapping is implemented but read mapping is not

## Architecture notes

- `gb-clj.cpu.bits` holds `combine`/`split` (pure 8/16-bit byte helpers) with zero dependencies, so both `bus.clj` and `cpu/util.clj` can use them without a circular require. This pattern (dependency-free namespace for anything both `bus` and `cpu/util` need) is the way to resolve future cycles of the same shape — see how `timer.clj` also avoids requiring `bus.clj` for the same reason.
- `util/inc16` and `util/dec-r16` both have a single-register arity (`[gb-state r]`) for SP alongside the two-register arity for register pairs like BC/DE/HL — follow that pattern for any future SP-specific 16-bit helper.

## Test Status

- `01-special.gb` — **Passed** ✓
- `02-interrupts.gb` — **Passed** ✓ — was hanging in a `HALT` loop waiting on the timer interrupt; fixed by having `halty-walty`'s idle branch (nothing pending) call `(util/tick gb-state 4)` instead of returning state untouched. Previously, `HALT` froze `:cpu :t-cycles`, which froze `timer/tick`'s elapsed-cycle delta, which froze DIV/TIMA — so the timer interrupt the CPU was halted waiting for could never fire. Pre-existing bug in `cpu.clj`, only surfaced once TIMA existed to be waited on.
- `03-op sp,hl.gb` — **Passed** ✓
- `04-op r,imm.gb` — **In progress** — blocked on `0xDE SBC A,d8`.
