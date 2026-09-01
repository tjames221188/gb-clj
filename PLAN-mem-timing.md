# Plan: cycle-accurate memory timing

Created: 2026-08-20

Goal: pass `mem_timing/individual/01-read_timing.gb`, `02-write_timing.gb`, and
`03-modify_timing.gb`.

This is a **redistribution** of cycles that are already correct, not a re-derivation.
`instr_timing.gb` passes, which proves every instruction's *total* T-cycle count is right.
What's wrong is *when within the instruction* the memory accesses happen.

---

## The problem

`cpu/step` executes an entire instruction against a frozen snapshot, then flushes all its
T-cycles to the timer afterwards (`cpu.clj:103-107`). So from the timer's point of view,
every memory access in an instruction happens at the same instant — the instruction
boundary. There is zero intra-instruction time.

Actual failure output (captured 2026-08-20):

```
01-read_timing:   F0:2-3  FA:2-4  CB 46:2-3  CB 4E:2-3  CB 56:2-3
                  CB 5E:2-3  CB 66:2-3  CB 6E:2-3  CB 76:2-3  CB 7E:2-3
02-write_timing:  36:2-3  E0:2-3  EA:2-4
03-modify_timing: 35:0/0-2/3  34:0/0-2/3  CB 06:0/0-3/4  CB 0E:0/0-3/4
                  CB 16 CB 1E CB 26 CB 2E CB 36 CB 3E  (all 0/0-3/4)
                  CB 86 CB 8E CB 96 CB 9E CB A6 CB AE CB B6 CB BE  (all 0/0-3/4)
                  CB C6 CB CE CB D6 CB DE CB E6 CB EE CB F6 CB FE  (all 0/0-3/4)
```

Format is `opcode:tested-correct` for read/write, and
`opcode:tested-read/tested-write-correct-read/correct-write` for read-modify-write.

Two things this proves:

1. **Every failure reports `2`**, regardless of instruction. The measured position is not a
   function of the instruction at all — exactly the signature of a frozen timer.
2. **Everything whose correct answer is `2` passes** (`LD A,(HL)`, `LD (BC),A`,
   `ADD A,(HL)`, `LD (HL),r`, `LD A,(C)`, …). All the 2-M-cycle memory ops pass *by
   accident*. Only accesses on M-cycle 3 or 4 fail.
3. `0/0` in modify_timing means **indeterminate** — read-modify-write needs the read and
   write observed at *different* timer states. Ours are at the identical instant, so the
   test can't infer either one.

---

## The rule (derived from the data above, not guessed)

Blargg's reported position maps to elapsed T-cycles as `position = elapsed/4 + 2`.
Working that back through every failing case:

| Op       | M-cycles | Access          | Needs elapsed |
|----------|----------|-----------------|---------------|
| `F0`     | 3        | read on M3      | 4             |
| `FA`     | 4        | read on M4      | 8             |
| `36`     | 3        | write on M3     | 4             |
| `E0`     | 3        | write on M3     | 4             |
| `EA`     | 4        | write on M4     | 8             |
| `34`/`35`| 3        | read M2, write M3 | 0, 4        |
| `CB 46`  | 3        | read on M3      | 4             |
| `CB 06`  | 4        | read M3, write M4 | 4, 8        |

Every one fits `elapsed = 4 × (N - 2)` for an access on M-cycle N. That gives one
implementable rule:

> **The opcode fetch is free** — its M-cycle is overlapped with the previous instruction.
> **Every other M-cycle is: do the access, then tick 4.**
> Then **one extra tick 4** at the end of the instruction to pay back the overlap.

Sanity checks:

- `NOP` — fetch (free) + trailing 4 = **4** ✓
- `LD A,(HL)` — fetch, read @ elapsed 0 (pos 2 ✓), tick 4, trailing 4 = **8** ✓
- `INC BC` — fetch, idle tick 4, trailing 4 = **8** ✓
- `PUSH BC` — fetch, idle→4, write hi @4, write lo @8, trailing = **16** ✓
- `CALL nn` — fetch, lo@0, hi@4, idle, write hi@12, write lo@16, trailing = **24** ✓

---

## Steps

### Step 0 — Make the feedback loop fast (~15 min)

A failing mem_timing run currently takes 2.5 minutes: it grinds to the 20M step cap, then
dumps 200 trace lines per ROM.

In `test/gb_clj/cpu_test.clj`, change `run-test-rom`'s loop to also stop on `"Failed"`, and
print `:serial-out` on failure. Drop the `cpu/dump-trace` calls from the ROM tests.

**Done when:** the mem_timing test finishes in seconds and prints the `F0:2-3 FA:2-4 …`
lines directly in the failure output.

Do this first — every step below is verified by re-reading those lines.

---

### Step 1 — Make `tick` advance the timer (~30 min)

New namespace `gb-clj.machine`, requiring `bus` and `timer`:

```clojure
(defn tick [st n]
  (-> (update-in st [:cpu :t-cycles] + n)
      (timer/tick n)))
```

Then delete the timer call from `cpu/step` (`cpu.clj:103-107`), and make `util/tick` a
one-line delegate to `machine/tick`. Don't touch the ~250 call sites — the delegate covers
them.

No dependency cycle: `util → machine → bus → timer`.

**Done when:** `instr_timing` still passes (totals unchanged).

**Expect exactly one regression, and it's the right kind:** `execute 0xCB`
(`instructions.clj:1291-1298`) is the only place that ticks *before* a memory access, so
the CB ops will move from reporting `2` to reporting `4`. Everything else is
read-then-tick already, so it stays put. **If anything else moves, stop and find out why.**

---

### Step 2 — Add the M-cycle primitives (~30 min)

Still in `gb-clj.machine`. Reads become stateful — this is the part that has to return a pair.

```clojure
(defn m-idle [st] (tick st 4))

(defn m-read [st addr]
  [(tick st 4) (bus/read-byte st addr)])   ; read the pre-tick state, then advance

(defn m-write [st addr v]
  (-> (bus/write-byte st addr v) (tick 4)))

(defn fetch-byte [st]                       ; operand fetch: read at PC, PC++, tick
  (let [pc (get-in st [:cpu :pc])]
    [(-> (assoc-in st [:cpu :pc] (bit-and 0xFFFF (inc pc))) (tick 4))
     (bus/read-byte st pc)]))
```

**Done when:** it compiles and all tests sit exactly where step 1 left them. Nothing calls
these yet.

---

### Step 3 — Migrate ONE opcode as proof of the model (~20 min)

Do `0xF0` alone. This is the moment that validates everything above.

Migration wrinkle: `step-cpu` fetches the opcode without incrementing PC, and unmigrated
opcodes still do `(inc-pc 2)` at the end. So a migrated opcode steps past its own opcode
byte first:

```clojure
(defmethod execute 0xF0 LDH_A_ADDR_A8
  [gb-state _]
  (let [st       (util/inc-pc gb-state)          ; M1 already done by step-cpu
        [st n]   (m/fetch-byte st)               ; M2 — access at elapsed 0
        [st val] (m/m-read st (+ 0xFF00 n))]     ; M3 — access at elapsed 4
    (-> (assoc-in st [:cpu :a] val)
        (m/tick 4))))                            ; overlap payback → 12 total
```

**Done when:** `F0:2-3` disappears from `01-read_timing` and `instr_timing` still passes.

If this works, the model is proven and the rest is mechanical. **If it doesn't, do not
proceed** — the rule is wrong and it's far cheaper to find that out on one opcode.

---

### Step 4 — Finish `01-read_timing` (~1 hr)

Remaining: `FA` (4 M-cycles: opcode, lo, hi, read) and `BIT b,(HL)`.

The CB ops need `execute 0xCB` restructured: `inc-pc` past the `CB`, then `fetch-byte` the
sub-opcode (that's M2, ticks 4), then dispatch — instead of the current `(inc-pc 2)` +
`(tick 8)`. That changes all CB ops at once, so the three prefix code paths (`rotate`,
`:bit`, `bit-update` in `prefix_instructions.clj`) all need their trailing `(tick 4)` in
the same commit. Only ~4 places.

Arithmetic check: register `BIT b,r` = fetch-byte (4) + trailing (4) = 8 ✓.
`BIT b,(HL)` = fetch-byte (4) + m-read at elapsed 4 (pos 3 ✓) + trailing = 12 ✓.

**Done when:** `01-read_timing` passes.

---

### Step 5 — `02-write_timing` (~45 min)

Only three: `36` (`LD (HL),n`), `E0` (`LDH (a8),A`), `EA` (`LD (a16),A`).

`36` currently routes through `util/load8-immediate`, which does double duty for registers
and addresses. Give `0x36` its own body rather than trying to make that helper
cycle-aware.

**Done when:** `02-write_timing` passes.

---

### Step 6 — `03-modify_timing` (~45 min)

`34`, `35`, and the CB `(HL)` read-modify-writes (already restructured by step 4 — they
just need `m-read`/`m-write` inside `prefix/rotate` and `bit-update`).

```clojure
(defmethod execute 0x34 INC_ADDR_HL [gb-state _]
  (let [st        (util/inc-pc gb-state)
        addr      (util/get16 st :h :l)
        [st prev] (m/m-read st addr)          ; M2 @ elapsed 0 → pos 2
        [val st]  (util/inc8 st prev)]
    (-> (m/m-write st addr val)               ; M3 @ elapsed 4 → pos 3
        (m/tick 4))))
```

**Done when:** all three mem_timing ROMs pass.

---

### Step 7 — Reassess the timer (do NOT pre-emptively rewrite it)

The `tima-counter` accumulator in `timer.clj` gives identical results at 4-T granularity as
at 12-T granularity in steady state, so it is **probably not** a blocker for these three
ROMs. Get to step 6, see what's still red, then decide.

If/when it does need rewriting (likely gated by `mem_timing-2` and `interrupt_time`):

- Replace the `tima-counter` accumulator with **falling-edge detection** on bit 9/3/5/7
  (per TAC 0/1/2/3) of the 16-bit `div-counter`, ANDed with TAC bit 2. The accumulator
  diverges from hardware specifically around **DIV writes** (which can produce a spurious
  TIMA increment — `div-write` currently resets `div-counter` but leaves `tima-counter`
  untouched) and **TAC changes**.
- Add the **delayed TIMA reload**: on overflow TIMA reads `0x00` for 4 T-cycles, *then*
  TMA is loaded and the interrupt fires. Writing TIMA during that window cancels the
  reload. `tima-inc` (`timer.clj:27-33`) currently reloads instantly.

---

### Step 8 — Independent bugs (none block the above; do whenever)

1. **Halt bug is an infinite loop.** `execute 0x76` sets `:halt-bug? true` without
   incrementing PC (`instructions.clj:741-751`), so the next step re-fetches `0x76` at the
   same PC, `do-halt-bug` (`cpu.clj:73-78`) executes `0x76` again, re-arms the same state,
   and restores PC. Nothing advances — **`halt_bug.gb` will hang the emulator today.**
   Also `do-halt-bug`'s model is wrong even with the PC fixed: the real bug is that the
   *opcode fetch* fails to increment PC (so a following `LD A,n` reads its own opcode byte
   as the operand), not that the instruction runs and PC rewinds. Best done *after* the
   migration, when `fetch-byte` owns the PC increment and this collapses to a flag that
   suppresses one increment.
2. **Echo RAM reads.** `write-byte` mirrors `0xE000-0xFDFF` down to `0xC000`
   (`bus.clj:35-36`) but `read-byte` reads the raw slot, which nothing ever writes — so
   reads return 0. Extract a shared `decode-addr` so read and write can't disagree.
3. **IME should start `false`** (`cpu.clj:35` has `true`). Also: post-boot IO register
   values (`0xFF00`→`0xCF`, `0xFF0F`→`0xE1`, `0xFF40`→`0x91`, `0xFF47`→`0xFC`) and DIV
   starting around `0xABCC`.
4. **IO read masking** — TAC should read back `0xF8 | val`, IF `0xE0 | val`, unmapped
   registers `0xFF`. Currently everything reads raw.
5. **`0x10 STOP`** — the only unimplemented opcode.
6. **Duplicated arithmetic** — `0xC6 ADD_A_N` / `0xD6 SUB_A_N` hand-roll flag logic with
   `util/half-carry?` instead of calling `util/add-val`/`util/sub-val`; `compare-val` is a
   third implementation of subtract-flags; `0x30 JR_NC_N` is hand-rolled while
   `0x18/20/28/38` use `jump-relative-pred-r8`. All currently correct, all divergence risks.
7. **No MBC** — writes to `0x0000-0x7FFF` are the mapper control interface but
   `bus.clj:27-30` drops them with a `log/error`. Individual test ROMs are 32KB so it
   doesn't bite yet, but `cpu_instrs.gb` (the combined ROM, already in `test-resources/`)
   is MBC1 and won't run, nor will any real game.
8. **No PPU / `0xFF44` (LY) is 0 forever.** Anything polling for VBlank hangs.

---

## Notes for later

### The other ~240 opcodes

You do **not** need to migrate them for mem_timing. The 2-M-cycle memory ops pass correctly
today, and register-only ops have no observable access position. Roughly **ten code paths**
get all three ROMs green.

Full migration matters later — `interrupt_time.gb`, `mem_timing-2`, and eventually a PPU.
At that point: hoist the `inc-pc` and the trailing `tick 4` into `step-cpu`, and probably
add a threading macro so each opcode reads as a list of M-cycles, e.g.

```clojure
(defop 0xF0 LDH_A_a8 [st]
  (m-let [n (fetch-byte)
          v (read-byte (+ 0xFF00 n))]
    (set-reg :a v)))
```

Do that once the pattern has proven itself across ten instructions, not before.

### Get a proper oracle before the full migration

[SingleStepTests/sm83](https://github.com/SingleStepTests/sm83) gives, per opcode, initial
state → final state **plus the exact ordered list of bus cycles (address, value,
read/write) per M-cycle**. It's the only oracle that checks M-cycle decomposition directly
rather than inferring it — far better than bisecting through Blargg for 240 opcodes.

### Internal (non-bus) M-cycles

These have no memory access and currently hide inside the blanket tick. Needed once
migration goes wide:

| Instruction                          | M-cycles | Internal |
|--------------------------------------|----------|----------|
| `INC BC` / `ADD HL,rr` / `LD SP,HL`  | 2        | 1        |
| `JR e` taken                         | 3        | 1        |
| `POP rr`                             | 3        | 0        |
| `JP nn` taken / `PUSH rr` / `RST`    | 4        | 1        |
| `ADD SP,e8`                          | 4        | 2        |
| `RET cc` taken                       | 5        | 2        |
| `CALL nn`                            | 6        | 1        |

### Performance

The mem_timing run took **221s CPU for ~60M steps (~270k instructions/sec)**. A DMG runs
~1M/sec, so this is ~0.3x real-time *before* per-M-cycle ticking (3-4x more work per
instruction) or a PPU.

Cause: `:memory` as a 65536-element persistent vector — every write is an `assoc-in`
path-copying through ~4 levels of 32-way branching. This is the one place the
all-immutable design genuinely hurts. The pragmatic fix is a Java `byte-array` for
`:memory` held inside the otherwise-immutable state map: keeps the threading style and the
pure CPU state, gives up structural sharing only for the thing that's a flat mutable array
on real hardware anyway. Benchmark before committing — it's a deliberate philosophical
compromise, not something to discover accidentally later.

### Unwired ROMs already in `test-resources/`

`mem_timing-2/`, `halt_bug.gb`, `interrupt_time.gb`, `oam_bug/`, `dmg_sound/`,
`cgb_sound/`, and `cpu_instrs/cpu_instrs.gb` (the combined MBC1 ROM).
