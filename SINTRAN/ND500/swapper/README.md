# ND-500 Swapper (SWAPPER-K01) - Binaries and Reverse-Engineering Analysis

The ND-500 swapper domain (SWAPPER-K01): its program and data segment binaries, the
resident ND-500 monitor symbol table, the current disassembly, and the RE write-ups.

> **Path convention.** Every file reference in this folder and its documents is written
> as a path from the repository root `E:\Dev\Ronny\NDInsight`
> (for example `SINTRAN/ND500/swapper/swapper-k01.pseg.md`). No bare filenames, no
> absolute host paths, no `../` links.

---

## Start here

Read `SINTRAN/ND500/swapper/swapper-k01-deep-analysis.md` first. It is the end-to-end
deep dive (role determination, MON 377B descriptor decode, request/response loop,
5MPM / front-door mapping) and it supersedes and partly corrects the older write-ups now
kept under `SINTRAN/ND500/old/`.

## What the swapper is

SWAPPER-K01 is the **ND-500-side paging/swap worker** - a native ND-500 domain
(`{PSEG, DSEG}` based at `0x08000000`) and a **CLIENT of SINTRAN**, NOT the low-level
handler that does ND-500 work for the ND-100. From the byte-level analysis:

**What it does**

- **Receives work as messages** posted into ND-100 private memory and pulls them across by
  **RIOM DMA** (3 sites in the PSEG). It is the ND-500 side of ND-500 **process #0**.
- **Dispatches each request on its own private 29-entry function-code table** (a `jumpg`
  through a table in the DSEG) - a per-request function code keyed into its own handler
  set, not a MON-number dispatcher.
- **Does the page moves itself, in-domain**, with the ND-500 physical-segment page
  primitives: **RPHS** (read physical segment, x2), plus **PCTSB** (x3) and **DCTSB** (x4)
  translation-buffer operations. This is the actual paging/swap work.
- **Traps outward to the ND-100 / SINTRAN only** when it needs a cross-machine service.
  Every **MON 377B** (15 sites) is an outward trap into logical segment 31
  (`0xF80000FF` = monitor call 255 = **N5SWAP**), used for e.g. swap-disk transfer and
  fatal-error reporting. It terminates via **MON 0B** (LEAVE).

**What it is NOT**

- It does **not** touch the bus-interface hardware - the 3022/5015 IOX registers are
  ND-100 I/O space it cannot reach.
- It has **no receive-side MON dispatcher** - every trap direction is outward; it is a
  service *requester*, not the trap target for ND-500 monitor calls.
- The actual swap-disk I/O is done by a **separate ND-100 RT-program (5SWAP)**, not by
  this domain.

**Two corrections the deep analysis established against earlier notes:** WPHS is absent
(only RPHS is present), and the pre-trap "internal call with identical args" is a
trace/log routine gated by a flag, not a try-local-then-forward fast path.

---

## Canonical file set

| File | What it is |
|------|------------|
| `SINTRAN/ND500/swapper/swapper-k01-deep-analysis.md` | THE deep dive - start here. Role determination, MON 377B descriptor decode, request/response loop, 5MPM / front-door mapping. |
| `SINTRAN/ND500/swapper/swapper-k01-handlers.md` | The 29-entry function-code dispatch table decoded: each `MSW*` code -> handler target -> behaviour (27 of 29 PROVEN-shape). The ND-100 side that sets the code (`SWPST`) is in `SINTRAN/ND500/ND500-SWAPPER-ANALYSIS.md` section 12. |
| `SINTRAN/ND500/swapper/swapper-k01.pseg.md` | Pseudo-C analysis of the program segment: routine inventory and the MON 377B gate mechanism. |
| `SINTRAN/ND500/swapper/swapper-k01.dseg.md` | DSEG hex/string dump plus the PSEG -> DSEG cross-reference. |
| `SINTRAN/ND500/swapper/swapper-k01-pseg.asm` | ND-500 disassembly of the PSEG (base 0x08000000, 12046 lines). The current, richer listing. |
| `SINTRAN/ND500/swapper/SWAPPER-K01.PSEG` | ND-500 program segment binary (I-space, 38161 bytes). |
| `SINTRAN/ND500/swapper/SWAPPER-K01.DSEG` | ND-500 data segment binary (D-space, 218117 bytes). |
| `SINTRAN/ND500/swapper/N500-SYMBOLS.SYMB` | Resident ND-500 monitor symbol table (7157 symbols). |
| `SINTRAN/ND500/swapper/README.md` | This index. |

---

## Superseded / history

These three files used to live in this folder and were absorbed and partly corrected by
the canonical set above; they now live under `SINTRAN/ND500/old/`. Notable corrections:
WPHS is absent (only RPHS is present), RIOM DMA does exist, and the pre-trap "internal
call with identical args" is a trace/log routine, not a fast-path.

- `SINTRAN/ND500/old/SWAPPER-K01-ANALYSIS.md` - the earlier reverse-engineering analysis.
- `SINTRAN/ND500/old/SWAPPER-MON-DISPATCH.md` - the earlier MON-dispatch write-up.
- `SINTRAN/ND500/old/SWAPPER-K01.PSEG.asm` - the older, plain disassembly.

---

## Live execution status (2026-07-20)

This swapper now **actually executes** on the RetroCore functional `CpuND500` under live SINTRAN L,
which turned several parts of this analysis into live-verified facts:

- **Link base confirmed:** the disassembly base `0x08000000` is real - the swapper runs in **logical
  segment 1**, with code and data in the SAME segment separated by the I/D split (program capability
  -> PSEG, data capability -> DSEG). SINTRAN itself places the executable at MPM physical `0x06F800`,
  byte-for-byte identical to `SWAPPER-K01.PSEG`.
- **Entry sequence confirmed instruction-for-instruction:** `init $1000441124` = `0x08024254` (the
  stack bottom in section 5.1) and `call $1000100645` = `0x080081A5` both matched live traps exactly.
- **The RIOM intake is where it currently stops:** `1000101356: h riom $1000440264,$1000440274,
  $1000440074+` (= `0x240B4`/`0x240BC`/`0x2408C`) writes to address ~0 because **the DSEG content is
  never loaded** - `0x24800` stays zero although the data page table reserves 107 pages
  (`0x35800` = 219,136 bytes vs `SWAPPER-K01.DSEG` = 218,117). The swapper is running against an
  empty data segment. How DSEG content is meant to be delivered is the open question.
- **Retired prior:** "the swapper is control-store microcode" was wrong. Microcode is only what
  "> Loading Control Store" puts in the CPU's control storage; this swapper is ordinary ND-500
  executable code.

Details: [`../ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md`](../ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md)
sections 12d-12j; status of record [`../ND500-STATUS-AND-INDEX.md`](../ND500-STATUS-AND-INDEX.md)
section 0g.

---

## Related

- `SINTRAN/ND500/ND500-SWAPPER-LOADING-MECHANISM.md` - **how SINTRAN loads the swapper (INZ500,
  MSINIT, 5SWRT)** - the first place to look for the missing DSEG-delivery step.
- `SINTRAN/ND500/ND500-SWAPPER-ANALYSIS.md` - swapper FIFO/queue mechanics from the ND-100 side.
- `SINTRAN/ND500/ND500-SWAPPER-LOADING-MECHANISM.md` - how SINTRAN loads the swapper (INZ500, MSINIT, 5SWRT).
- `SINTRAN/ND500/nd-500-mon/` - the ND-100-side ND-500/5000 monitor (the other end of the MON 377B gate and the MON 60 front door).
- `SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md` - the ND-100 <-> ND-500 bus interface reference.

---

**Parent:** `SINTRAN/ND500/README.md`
