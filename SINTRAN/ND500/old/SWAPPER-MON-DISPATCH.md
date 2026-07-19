# SWAPPER-K01 - Monitor-Call Dispatch: Is the Swapper the Universal MON Dispatcher?

**Date:** 2026-07-09
**Question tested:** Does the ND-500 Swapper receive and dispatch **all** monitor
calls issued by ND-500 user programs, deciding which to service locally (in ND-500
code) versus which to forward to the ND-100 SINTRAN kernel?

**Answer: NO - hypothesis is FALSE (VERIFIED).**
The swapper is a specialised paging/swap worker domain and a *client* of SINTRAN.
It is not the trap target for ND-500 MON calls and contains no MON-number dispatch
table. The universal ND-500 MON first-responder is the resident **ND-500 System
Monitor** (whose symbols are in `N500-SYMBOLS.SYMB`), assisted on the ND-100 side by
the driver `MP-P2-N500.NPL` (`GOSW` for MON 500-523, `NORMMC` for MON < 500).

Sources: [`SWAPPER-K01.PSEG.asm`](SWAPPER-K01.PSEG.asm),
[`N500-SYMBOLS.SYMB`](N500-SYMBOLS.SYMB),
[`SWAPPER-K01-ANALYSIS.md`](SWAPPER-K01-ANALYSIS.md).
Cross-ref: [`../ND500-MONITOR-CALL-PARAMETER-PASSING.md`](../ND500-MONITOR-CALL-PARAMETER-PASSING.md),
[`../ND500-BUS-INTERFACE-REFERENCE.md`](../ND500-BUS-INTERFACE-REFERENCE.md).

---

## 1. Is there a MON trap handler in the swapper? (NO)

**VERIFIED.** The swapper's PSEG issues MON traps; it does not *receive* them.
Grep of every MON trap target (`call $0xFFFFFFFFF80000xx`) in the 12046-line
disassembly returns exactly two distinct targets:

| Trap target | MON | Count | Role |
|---|---|---|---|
| `F8000000` | MON 0B (EXITT/leave) | 1 | revision-mismatch guard, `.asm` line 40 |
| `F80000FF` | MON 377B (SINTRAN monitor-call gateway) | 15 | all real SINTRAN service calls |

There is **no** MON entry vector, no trap prologue keyed to a MON number, and no
reference anywhere to MON numbers 410-427, 500-515, 231, 347, or 255. The swapper's
only entry is the domain prologue `INIT` at `.asm` line 16 followed by the
`REV.-K01` self-check; it then runs as a **message-driven server**, not a trap
handler. (See ANALYSIS sec 4-5.)

Consequence: an ND-500 user program's MON call cannot land in the swapper first.
It traps to the resident ND-500 System Monitor.

---

## 2. The swapper's actual dispatch table (keyed by MESSAGE function code, not MON number)

**VERIFIED.** The swapper is driven by messages posted by the ND-100 (RT-program
`5SWAP` / driver `5P-P2-MON60.NPL`). Its dispatcher reads a **message function
code** from DSEG word `0x080240B8` and does an indexed jump:

```
.asm 10598:  w comp2 $0x80240B8,$0x5      ; special-case function code 5
.asm 10600:  w comp2 $0x80240B8,$0x3      ; special-case function code 3
.asm 10602:  w comp2 $0x80128E4,$0x14     ; bound check: max index = 0x14 (20)
.asm 10604:  w1 := $0x80240B8             ; index := message function code
.asm 10605:  jumpg $0x8026198+            ; jump via handler table at DSEG 0x26190
```

The table at DSEG `0x26190` is ~29 I-space (PSEG) code pointers, all in the
`0x08008xxx` region (enumerated in ANALYSIS sec 6). The index range is **0..20**,
i.e. a small set of swapper commands - it is emphatically **not** the MON-call
number space. This is the swapper's private command protocol, not a MON dispatcher.

UNCERTAIN: the full per-command semantics of the 29 handlers (only their PSEG
addresses and the dispatch mechanism are established; ANALYSIS sec 10).

---

## 3. Local-vs-forward decision inside the swapper

**VERIFIED (mechanism).** The split is between *paging work the swapper does with
its own ND-500 instructions* and *disk/OS work it forwards to SINTRAN via MON 377B*.

**Local (ND-500 code):**
- Page moves with the '87-architecture primitives **RPHS** (read from physical
  segment = swap-in, `.asm` 1389/1436) and **WPHS** (write to physical segment =
  swap-out). ANALYSIS sec 7.
- Page-control-table maintenance with **PCTSB / DCTSB** (`.asm` 94, 111) and the
  in-DSEG table/buffer updates.

**Forward (to ND-100 SINTRAN):** every `MON 377B`. The forwarding "message" is the
argument vector of the trap itself:

```
call MON377B , <nargs> , <selector-ptr@0x12A2x> , <data buffer> ...
```

The selector pointer indexes the small DSEG table at `0x12A20-0x12A3C` (ANALYSIS
sec 5.2). The dominant forward (selector `0x12A24`, 7 args, fixed buffer
`$0x8014CF8`) is the **page-transfer request** the ND-100 turns into a disk `ABSTR`
via 5SWAP. The error path forwards selector `0x12A20 = 0x427 = SWPFATAL (2047B)`
("FATAL ERROR FROM SWAPPER", `.asm` line 520).

**The "try-local-else-forward" pattern** is real but narrow (a specific
paging/allocation step, not a MON router). At `.asm` 102-104:

```
.asm 102:  call $0x8009381,$0x2,$0x8012A34,$0x8023D7C  ; internal attempt
.asm 103:  ifkret                                       ; if handled (K flag) -> return
.asm 104:  call MON377B ,$0x2,$0x8012A34,$0x8023D7C     ; else forward to SINTRAN
```

So `ifkret` (return-if-K) implements the local-first / forward-on-miss choice for
that one operation. This is the swapper acting as a SINTRAN client, not as a
general MON dispatcher.

Message field offsets it/ SINTRAN use (from `N500-SYMBOLS.SYMB`, VERIFIED):
`N5STA=2`, `SENDE=3`, `X5CPU=4`, `X5ACT=5`, `MICFU=6`, `NUMPA=012`, `MCNO=013`,
`MSWMC=014`, `SMCNO=037`, `STOPR=011`. These are the ND-100<->ND-500 message
fields; the swapper's own buffers are zeroed BSS filled at runtime, so the runtime
`MICFU`/status values it writes are not statically present (ANALYSIS sec 9-10,
UNCERTAIN).

---

## 4. Classification of the specific monitor calls

For each, the question is "does the **swapper** handle it locally or forward it?"
The verified answer for **all** of them is the same: **the swapper does not dispatch
any of these by MON number at all** - none appears in the PSEG. They are dispatched
by the ND-500 System Monitor (resident) and/or the ND-100 driver `MP-P2-N500.NPL`.
The swapper only ever participates as the *target of a swap message* (for the
swapper-monitor-call family) or as a *SINTRAN client* (MON 377B).

| MON | Name | Dispatched by | Swapper role |
|---|---|---|---|
| 410 | FIXSEG | Sys Monitor / ND-100 (`< 500` -> NORMMC) | none in PSEG |
| 411 | UNFIX (`UNFIX=112463` is unrelated symbol) | Sys Monitor / ND-100 | none in PSEG |
| 416 | WSEGN | Sys Monitor / ND-100 | none in PSEG |
| 417 | MXPISG | Sys Monitor / ND-100 | none in PSEG |
| 425 | SPRNAME (`SPRNA`) | Sys Monitor / ND-100 | none in PSEG |
| 426 | GPRNUM | Sys Monitor / ND-100 | none in PSEG |
| 427 | GPRNAME (`GPRNA`) | Sys Monitor / ND-100 | none in PSEG |
| 500/501 | SWMC (swapper monitor call) | ND-100 `GOSW` (500-523) -> **routed to swapper as a message** | receives as message; handles via the 0x26190 function-code dispatch (NOT via MON number) |
| 505 | SWMC family | ND-100 `GOSW` -> swapper message | as above |
| 510 | SWMC family (`LM500=000510`) | ND-100 `GOSW` -> swapper message | as above |
| 511 | DVIO | ND-100 `GOSW` (level 12) | none in PSEG |
| 512 | (A5XMSG per ND-100 ctx) | ND-100 `GOSW` | none in PSEG |
| 513 | B5XMSG | ND-100 `GOSW` | none in PSEG |
| 515 | 5MTRANS | ND-100 `GOSW` | none in PSEG |
| 255 | N5SWAP ("swapper internal") | Sys Monitor / ND-100 -> swapper message | receives as message; not a swapper-side MON handler |
| 231 | nucleus | ND-500 System Monitor | none in PSEG |
| 347 | nucleus | ND-500 System Monitor | none in PSEG |

Key nuance for the **SWMC / N5SWAP family (500/501/505/510, 255)**: these *do* end
up at the swapper, but not because the swapper dispatched a MON number. The System
Monitor / ND-100 driver recognises them, builds a message (setting `MSWMC` off 014,
`SMCNO` off 037, `MCNO` off 013), and posts it to the swapper's mailbox. The swapper
then dispatches on its own **message function code** (DSEG 0x240B8), which is a
different, smaller namespace than the MON numbers. VERIFIED that the swapper has no
MON-number switch; UNCERTAIN is the exact function-code -> SWMC-subfunction mapping
(needs the ND-100 message-build side of `MP-P2-N500`/`5P-P2-MON60`).

---

## 5. Conclusion

**The user's hypothesis is FALSE (VERIFIED).** The swapper is **not** the universal
first-responder/dispatcher that sees every ND-500 MON call and decides local vs
forward. Evidence:

1. The PSEG issues MON traps (15x MON 377B, 1x MON 0B) but **receives none** - it
   has no MON trap entry and no MON-number dispatch table.
2. Its only dispatcher keys on a private **message function code** (DSEG 0x240B8,
   index 0-20) via the handler table at DSEG 0x26190 - a swap-command namespace,
   not the MON-call namespace.
3. Its local-vs-forward logic is the narrow "do paging with RPHS/WPHS/PCTSB, else
   MON 377B to SINTRAN for disk I/O and error reporting" client pattern.

**What actually owns the universal MON-dispatch role:** the resident **ND-500
System Monitor** (symbols in `N500-SYMBOLS.SYMB`) is the trap target for ND-500 user
MON calls; it services many locally and forwards the rest to the ND-100 by building
a message (`MCNO`/`MICFU`/`NUMPA`), where `MP-P2-N500.NPL` dispatches MON 500-523 via
`GOSW` on level 12 and MON < 500 via `NORMMC`.

**What the swapper does NOT handle:** general MON dispatch, and specifically none of
MON 410/411/416/417/425/426/427/511/512/513/515/231/347 (no handler keyed by these).
It **does** participate as the message *target* for the swapper-monitor-call /
N5SWAP family (500/501/505/510, 255) and as a SINTRAN *client* for disk page
transfers and fatal-error reporting - both via mechanisms other than MON-number
dispatch.

---

**Verification status:** MON-trap inventory, message-code dispatcher, RPHS/WPHS
primitives, MON 377B forwarding, and message-field offsets are VERIFIED against the
`.asm` and `N500-SYMBOLS.SYMB`. The per-command handler semantics and the runtime
`MICFU`/status values are UNCERTAIN (runtime BSS; ANALYSIS sec 10).
