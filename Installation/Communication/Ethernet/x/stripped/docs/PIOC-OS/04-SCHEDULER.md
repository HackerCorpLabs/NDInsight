# PIOC-OS - Scheduler and context switching

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: core mechanism decoded. Queue link structure and the idle path are partial.

---

## 1. Summary

PIOC-OS is **priority-preemptive with deferred switching**. It never switches inside the code that
makes a process runnable. Instead it sets a flag and, when that path is an interrupt, **rewrites
the interrupt's own saved PC and SR** so the `rte` lands in the scheduler instead of resuming the
interrupted instruction stream.

That is the textbook "schedule on interrupt exit" design, and it is what makes the system genuinely
preemptive rather than a state machine or a cooperative round-robin.

---

## 2. The globals

| Address | Size | Meaning |
|---|---|---|
| **0x0650** | long | **current process descriptor** pointer. Zero means "no current process" |
| **0x0660** | byte | **reschedule pending**. `bset #0` - the set-and-test guard |
| **0x0662** | byte | second latch, guards the stack-frame rewrite (see section 3) |
| **0x0658** | long | pointer just above the interrupted exception frame |
| **0x0B06** | 16 longs | **ready-queue heads, one per priority level** |
| 0x0666 | — | kernel stack base; trap #2 switches to `0x0666 + 0x3FE` |

**Caution:** two of these were previously named `trap_hook_ctx_ptr` and `trap_hook_lock`. Those
names are wrong - they are the current-process pointer and the reschedule flag.

---

## 3. Deferred preemption - the return-address hijack (0x2192)

Called whenever an event post unblocks a process (from `0x2562`, the event poster).

```
bset #0,(0x0660)          set reschedule-pending
bne  done                 ALREADY pending - nothing more to do
bset #0,(0x0662)          second latch
bne  done                 frame already rewritten - do not rewrite twice
      movem.l {A0/A1},-(SP)
      A1 = (0x0658)               the interrupted exception frame
      A0 = (0x0650)               the current process descriptor
      move.l (-0x4,A1),(0x70,A0)  SAVE the interrupted PC  -> desc+0x70
      move.w (-0x6,A1),(0x74,A0)  SAVE the interrupted SR  -> desc+0x74
      lea    (0x2C90,PC),A0
      move.l A0,(-0x4,A1)         REPLACE the return PC with the scheduler
      move.w #0x2100,(-0x6,A1)    REPLACE the SR: supervisor, IPL 1
      movem.l (SP)+,{A0/A1}
done: rts
```

So the interrupt returns *into* `0x2C90` at IPL 1, and the work it was going to resume is recorded
in the descriptor to be restored later like any other context.

**The double-latch matters.** `0x0660` alone would let a second event post rewrite the frame a
second time, which would overwrite the saved PC with the scheduler's own address and lose the
interrupted context permanently. `0x0662` is what prevents that. An emulator that reimplements this
must keep both.

---

## 4. The scheduler proper (0x2C90)

```
move.l A0,-(SP)                     stash A0, it is needed as scratch
A0 = (0x0650)                       current descriptor
movem.l {D0-D7/A0-A6},(0x30,A0)     save 15 registers into the descriptor
move USP,A1 ; move.l A1,(0x6c,A0)   save the user stack pointer
move.l (SP)+,(0x50,A0)              patch the saved A0 slot with the REAL A0
if ((0x0650) != 0) A0 = (0x0650)
clr.b (0x0660)                      clear reschedule-pending
A0 = 0x0B06                         the ready-queue head array
D0 = 0x40
loop: D0 -= 4
      if (D0 < 0) goto idle         no runnable process at any priority
      A1 = *(A0 + D0)               head of this priority's queue
      if (A1 == 0) continue         empty - try the next level down
      ...dispatch this process...
```

### The ready-queue array

`0x0B06`, **16 longwords** (0x40 bytes), indexed by priority. The scan **starts at the top
(D0 = 0x40) and works down**, so the highest-numbered priority wins. That matches process creation,
where priority is validated 1..15 and RTC is created at 14 while PRO1 sits at 1.

An empty level is a NULL head and is skipped. Falling off the bottom means nothing is runnable,
which is what the FREE process (priority 5, entry 0x1A96) exists to prevent.

---

## 5. The descriptor context frame

Assembled from both routines above, and self-consistent:

| Offset | Size | Content |
|---|---|---|
| +0x30 | 32 | D0-D7 |
| +0x50 | 28 | A0-A6 (the `+0x50` write patches the A0 slot - `0x50-0x30 = 0x20` = register index 8 = A0, which confirms the layout) |
| +0x6C | 4 | USP |
| **+0x70** | 4 | **saved PC** - the interrupted return address, or at creation the process entry point |
| **+0x74** | 2 | **saved SR** |

This explains an earlier observation that `desc[0x70] = desc[0x1e]` at start-up "reloads the saved
entry point": starting a process is just seeding the same PC slot the scheduler restores from.

Trap #2 saves to the same area (`(0x0650)+0x30`), so kernel entry and preemption share one context
format.

---

## 6. When does a switch actually happen?

| Trigger | Path | Preemptive? |
|---|---|---|
| Event post unblocks a higher-priority process | `0x2562` -> `0x2192` -> frame rewrite -> `rte` into 0x2C90 | **Yes**, deferred to interrupt exit |
| `PosStartProcess` (trap 0x04) on a higher-priority process | sets reschedule-pending after the priority comparison | Yes |
| Wait-for-event with nothing pending (traps 0x0A/0x0B) | sets flags bit 1 + `0x0660`, returns to the scheduler | Yes - voluntary block |
| Block-self (trap 0x19) | same, plus the SCIP doorbell to the host | Yes - voluntary block |
| MFP tick (vector 69, ISR 0x3A68) | bumps 0x0FC2/0x0FCA; timer expiry posts events, which reaches 0x2192 | Indirectly |

There is **no time-slicing between equal-priority processes** anywhere in this path. A process at a
given priority runs until it blocks or something higher priority becomes runnable. With only three
real processes at priorities 1, 5 and 14, that is a sane design rather than an oversight.

---

## 7. Open

- The **queue link field** inside the descriptor (how the ready list is threaded) is not yet
  identified; `PlancListAppendLast` is used by `PosStartProcess`, so it is likely the standard
  `#APPD` link at a fixed offset.
- The **dispatch tail** of 0x2C90 past the priority scan (how it installs the chosen descriptor as
  current and returns to it) is not yet transcribed.
- The **idle path** when no queue is non-empty.
- Flags bits: **bit 0** = created, **bit 1** = blocked, **bit 3** = waiting (trap 0x0A flavour),
  **bit 4** = started, **bit 5** = waiting (trap 0x0B flavour), **bit 2** = runnable (set by
  `PosStartProcess`). Bits 6+ unknown.

---

## 8. Correction: `tbl_piocOsSchedulerActionDispatch` is misnamed

The table at **0x0512** is NOT a scheduler table. Its dispatcher at 0x1BDA does:

```
lea (0x406).l,A0 ; move.w (0x2,A0),D0w      <- reads SUBFN at 0x0408
bounds-check 0..5, else reply -2
asl #2 ; lea (0x512).l,A2 ; jsr (A1)
```

It is the **ND-100 monitor request dispatch**, keyed on the host's SUBFN word, replying in the
monitor postbox at **0x040C** (1 on success, **-2** for an out-of-range SUBFN). Entries:

| SUBFN | Handler | Action |
|---|---|---|
| 0 | 0x1A10 | restore context and `rte` |
| 1 | 0x1C1A | clear 0x04C0, reply 1, call 0x1A48, restore context via 0x1A12 |
| 2,3,4 | 0x1C38 | reply 1 and return - a plain acknowledgement |
| 5 | 0x1C48 | reply 1, clear 0x04C0, call 0x1A48, then call through the pointer at 0x04B6 |

Suggested rename: `tbl_hostMonitorSubfnDispatch`. This is directly relevant to the HLE - it is the
host control interface, and the -2 reply is an observable behaviour.
