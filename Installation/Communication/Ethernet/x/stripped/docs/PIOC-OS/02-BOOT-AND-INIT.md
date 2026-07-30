# PIOC-OS - Boot and initialisation

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: reset entry and the warm/cold split decoded. The full subsystem init chain is PARTIAL.

---

## 1. Where execution begins

68000 vector 1 (address 0x004) holds **0x1CFE** - that is the entry point of the whole image.
Vector 0 (0x000) holds **0x05C8**, the initial supervisor stack pointer; the stack grows DOWN from
there and the module directory grows UP from the same address (see `01-MODULE-INVENTORY.md`).

At entry the CPU is in supervisor mode with interrupts masked at level 7.

---

## 2. `reset_entry` (0x1CFE) - and the warm-restart branch

```
(0x0500) = A0                       stash the incoming A0 - the monitor dispatcher reads it back
(0x040E) = 1                        \  handshake words in the shared comm block
(0x040C) = 0                         > see the MASTER REFERENCE for the block layout
(0x0406) = 0                        /  REQUEST cleared
A0 = (0x0500)

if ((0x04BA) == 0x55555555) {       /* WARM RESTART */
      (0x04BA) = 0                  clear the signature so the next reset is cold
      (0x04BE) += 1                 bump the restart counter
      jsr 0x1A30
      (0x040C) = 4                  post reply code 4 = "restarted"
      jsr 0x1A48
      jsr 0x1A12                    restore the saved context and resume
}
/* COLD START falls through to 0x1D58 */
move.l A0,-(SP)
A0 = 0x0454
movem.l {D0-D7/A0-A6},(A0)          save the full register set to the context frame
move.l (SP)+,(0x20,A0)              patch the saved A0
move USP,A1 ...
```

### What this gives you

| Address | Meaning |
|---|---|
| **0x04BA** | **warm-restart signature.** `0x55555555` means "resume, do not cold start". Cleared on use |
| **0x04BE** | **restart counter** - incremented on every warm restart. A non-zero value on a live card means the firmware has been restarted N times |
| **0x0500** | saved A0 from entry; read back by the monitor request dispatcher at 0x1BC8 |
| **0x040C** | monitor reply word. **4** = warm restart completed |
| 0x0454-0x049D | the 68000 context save frame (see section 4) |

The restart counter at 0x04BE is worth reading on any live card - it distinguishes "this firmware
booted once cleanly" from "something has been restarting it".

---

## 3. The context save frame (0x0454-0x049D)

Used by both the cold path here and the monitor stop/continue handlers.

| Offset | Content |
|---|---|
| +0x00 | D0-D7, A0-A6 via `movem.l` (the +0x20 slot is patched with the real A0) |
| +0x3C | USP |
| +0x40 | SSP |
| +0x44 | PC |
| +0x48 | SR |

`0x0494` / `0x0498` set to **-1** means "no valid saved context". Saved at 0x1A66 and 0x1D5A,
reloaded at 0x1A12 followed by `rte`.

Note this is a *different* structure from the per-process context area at `desc+0x30` used by the
scheduler (`04-SCHEDULER.md`). This one is the whole-machine frame for monitor stop/continue; that
one is per-process for task switching.

---

## 4. Board configuration - supplied by the host

Read once at boot (0x2740-0x2756) from the record at **0x04CA**:

| Offset | Meaning |
|---|---|
| +0x30 | DRAM size in **2KB pages** - used to compute the heap top |
| +0x32 | PIOC number -> stored at 0x064C (`PIOC_NUMBE`) |
| +0x34 | ND-100 CPU number -> stored at 0x064E (`ND100_CPU`) |

The card does not size its own memory; the host tells it. An emulator must populate this before the
heap initialiser runs or the heap top will be wrong.

---

## 5. Known init steps (order NOT fully established)

These are the initialisers identified so far. **The exact call order has not been traced** - listing
them is not a claim about sequence.

| Routine | Address | Does |
|---|---|---|
| `HeapInitFreeList` | 0x35BE | build the heap free list (see `08-MEMORY.md`) |
| `RtcInit...` | 0x3AD2 | program the MFP, create and start the RTC process |
| `POSIINITIALIZE` / `POSISTART` | — | create the sub-process layer |
| `PosCreateObject` | 0x2E98 | called exactly **three** times, all at boot |
| `INITLANCE` | 0x48EA | LANCE initialisation |
| `INITERRORS` | 0x7528 | error subsystem |
| `INITRESOUR` | 0x7776 | resource init |
| `INITSUPERK` | 0x76C4 | "super kernel" init |
| `LNMAINIT` | 0x6EAA | LNMA layer |
| `LNCNINIT` | 0xB46A | LNCN layer |
| `POCSSpCommInitialize` | 0xBB30 | POCS sub-process |

The three `PosCreateObject` call sites produce, in order: `FREE` (priority 5), `PRO1` (priority 1),
`RTC ` (priority 14). See `03-PROCESS-MODEL.md`.

---

## 6. PRKEY - the start gate

The ND-100 kernel routine `PISTA` (MON 255B, T=6 "START PIOC") busy-polls **word 1002B** of the
shared block for **PRKEY = 052163B**, and will not proceed until it appears.

Word 1002B = byte offset **0x404** (ND-100 logical word N maps to 68K byte 2N, so 1002B = 514
decimal -> 0x404). 052163B = 0x5473.

### RESOLVED 2026-07-27 - the writer is found AND it runs correctly

**The writer**: `PublishDatafieldTableAndPrkey` @ **0x1C6A**, whose last instruction before `rts` is

```
0x1CF4:  move.w #0x5473,(0x00000404).l
```

A byte search for `54 73` across all 524,288 bytes of the image returns **exactly one hit**, so this
is the only writer. The routine first builds the datafield pointer table at 0x04CA and writes PRKEY
last - PRKEY therefore means "the table is published AND I am alive", deliberately.

**Its single caller** is `jsr 0x1C6A` at **0x1DA6**, on the cold-start path:

```
0x1D9C  jsr 0x1AD4
0x1DA0  jsr 0x396A
0x1DA6  jsr 0x1C6A          publish table, then PRKEY
0x1DAA  clr.l (0x04BA)      clear the warm-restart signature
0x1DB0  (0x04C0) = 1        STARTED
0x1DBC  (0x040C) = 3        reply code 3
0x1DCA  jsr 0x1A48          raise SCIP
```

**And it works.** A full 68K PC trace under the C# emulator (2026-07-27, 44,731 lines, reaching
List-Routing-Info) shows `(0x04BA) == 0` at reset, the cold path taken in full, and PRKEY, STARTED
and reply 3 all written. Result:
`E:\Dev\Repos\Ronny\RetroCore\DOCS\PRKEY_INVESTIGATION_RESULT_2026-07-27.md`

**So finding F10 - "the firmware never posts PRKEY" - does NOT hold for this image under emulation.**
Whatever was observed on the real card was either a different condition or a host-side fault. The
ENNS0 "Unknown name" symptom turned out to be command ORDERING, fixed 2026-07-23 by defining the
remote host first.

**The one real risk that remains**, visible in the sequence above: `0x04BA` is cleared only at
0x1DAA, *on the cold path itself*. If that longword ever holds 0x55555555 at reset, the firmware
takes the warm branch at 0x1D30, posts reply 4, and returns without ever reaching 0x1DA6 - no PRKEY,
no STARTED. Not what happened here, but it is a genuine latent trap for any emulator that leaves
DRAM patterned.

---

## 7. Open

- The actual init call order from `reset_entry` onward
- Which routine writes PRKEY, and under what condition (section 6)
- What 0x1A30, 0x1A48 and 0x1A12 each do in detail (they appear in both the warm-restart path and
  the monitor SUBFN handlers)
- Where the initial `A0` at entry comes from and what it points at

---

## Provenance

Sections 2 and 3 were read from the image on 2026-07-26. Sections 4 and 6 are prior findings
re-stated. Section 5 is an inventory, explicitly not an ordering claim.
