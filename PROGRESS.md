# Implementation Progress

Last updated: 2026-04-23

## Unprefixed Opcodes

`✓` = implemented, `·` = missing (not yet needed or not yet written), `✗` = invalid opcode (hard-locks real hardware, safe to leave unimplemented)

```
     0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
0x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓
1x   ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
2x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·
3x   ✓    ✓    ✓    ·    ·    ✓    ✓    ·    ·    ✓    ✓    ✓    ✓    ✓    ✓    ·
4x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
5x   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
6x   ·    ·    ·    ·    ·    ·    ✓    ·    ·    ·    ·    ·    ·    ·    ✓    ✓
7x   ✓    ✓    ✓    ✓    ✓    ✓    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·
8x   ·    ·    ·    ·    ·    ·    ·    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
9x   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
Ax   ·    ·    ·    ·    ·    ·    ·    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Bx   ·    ✓    ·    ·    ·    ·    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓    ✓
Cx   ✓    ✓    ✓    ✓    ✓    ✓    ✓    ·    ✓    ✓    ✓    ✓    ·    ✓    ✓    ·
Dx   ✓    ✓    ✓    ✗    ·    ✓    ✓    ·    ✓    ·    ✓    ✗    ·    ✗    ·    ·
Ex   ✓    ✓    ✓    ✗    ✗    ✓    ✓    ·    ·    ✓    ✓    ✗    ✗    ✗    ✓    ·
Fx   ✓    ✓    ✓    ✓    ✗    ✓    ·    ·    ·    ·    ✓    ✓    ✗    ✗    ✓    ·
```

**Implemented: 143 / 245 valid opcodes (58%)**

### Notable missing unprefixed opcodes

| Opcode | Mnemonic | Notes |
|--------|----------|-------|
| `0x08` | `LD (a16),SP` | Store SP to memory |
| `0x10` | `STOP` | Low priority |
| `0x2F` | `CPL` | Complement A — next up after DAA |
| `0x33` | `INC SP` | |
| `0x34` | `INC (HL)` | |
| `0x37` | `SCF` | Set carry flag |
| `0x38` | `JR C,r8` | Missing conditional JR |
| `0x3F` | `CCF` | Complement carry flag |
| `0x60–0x65`,`0x67` | `LD H,r` | H-register load family |
| `0x68–0x6D` | `LD L,r` | L-register load family (partial — 0x6E/0x6F done) |
| `0x76` | `HALT` | Needs interrupt system |
| `0x7F` | `LD A,A` | Trivial no-op load |
| `0x80–0x87` | `ADD A,r` | Entire ADD family missing |
| `0x90–0x9F` | `SUB/SBC r` | Entire SUB/SBC families missing |
| `0xA0–0xA7` | `AND r` | Entire AND family missing |
| `0xB0`,`0xB2–0xB5` | `OR r` | Partial OR family |
| `0xCC` | `CALL Z,a16` | |
| `0xD4` | `CALL NC,a16` | |
| `0xD9` | `RETI` | Return + enable interrupts |
| `0xDC` | `CALL C,a16` | |
| `0xF6` | `OR d8` | |
| `0xF8` | `LD HL,SP+r8` | |
| `0xF9` | `LD SP,HL` | |
| `0xFB` | `EI` | Enable interrupts |
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

## Known Issues / TODOs

- No interrupt system (IME, IE, IF registers) — `HALT`, `RETI`, `EI` all blocked on this
- No timer (TIMA/TMA/TAC registers)
- No LCD/PPU — Blargg tests may need vblank timing
- `inc16` in `util.clj` only works for register pairs, not SP directly (comment in code)
- Echo RAM (0xE000–0xFDFF) write mapping is implemented but read mapping is not

## Test Status

Running `01-special.gb` (Blargg cpu_instrs): reaches ~186k steps before hitting unimplemented opcode. No serial output yet — ROM needs more opcodes to complete its initialisation and reach the test output phase.
