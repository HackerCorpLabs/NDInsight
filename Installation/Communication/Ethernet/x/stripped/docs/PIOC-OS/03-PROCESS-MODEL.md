# PIOC-OS - Process and object model

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: descriptor map complete for every field the kernel touches. Total descriptor size is
NOT statically recoverable - see section 6.

---

## 1. Two distinct layers - do not conflate them

| Layer | Created by | Appears in the object table? |
|---|---|---|
| **Processes** (schedulable, own context and priority) | `PosCreateObject` trap 0x03 | **Yes**, at 0x0A8A |
| **Sub-processes** (`LNMASPCOMM`, `LNMASPDATA`, `LNCNSPCOMM`, `POCSSPCOMM`) | `POSIINITIALIZE` / `POSISTART` | **No** |

There are exactly **three** processes. Everything else that looks like a task is a sub-process: a
message-queue endpoint serviced by one of those three, not a scheduled entity with its own stack
and priority. `CMDSERVICE` pulling work off `LNMASPCOMM` is a loop inside a process, not a context
switch.

---

## 2. The object table - `POS_OBJECT_TABLE` @ 0x0A8A

- **30 longword slots**, **1-based**: handle 1 is slot[0], so the address of a slot is
  `0x0A8A + (handle-1)*4`... **but the kernel indexes it directly as `0x0A8A + handle*4`** (see
  trap 0x05 and 0x2562, both of which `asl.w #2` the raw handle). Treat the table as 31 entries
  with slot 0 unused; handle bounds are checked as `0 <= h <= 0x1E`.
- Each non-zero slot points at a **heap-allocated descriptor record**, not a function.
- A free slot is zero. Trap 0x05 (delete) clears the slot.

Enumerating every process the firmware created, from a live dump:

```c
for (h = 0; h <= 30; h++) {
    desc = *(uint32 *)(0x0A8A + h*4);
    if (!desc) continue;
    name     = (char   *)(desc + 0x18);   /* 4 chars, e.g. "RTC " */
    handle   = *(uint16 *)(desc + 0x0E);
    flags    = *(uint16 *)(desc + 0x16);
    priority = *(uint8  *)(desc + 0x1C);  /* 1..15 */
    entry    = *(uint32 *)(desc + 0x1E);
}
```

---

## 3. The descriptor field map

Every offset below is used by code that has been read.

| Offset | Size | Field | Established from |
|---|---|---|---|
| +0x0E | 2 | handle (== its table index) | trap 0x11 |
| **+0x16** | 2 | **flags** - see section 4 | traps 0x0A/0x0B/0x19, poster 0x2562 |
| +0x18 | 4 | name, 4 chars | trap 0x08, `PosCreateObjectBody` |
| +0x1C | 1 | priority 1..15 | creation, scheduler |
| +0x1E | 4 | entry point | `RtcInit` parameter block, creation |
| +0x30 | 32 | saved D0-D7 | scheduler 0x2C90, trap #2 entry |
| +0x50 | 28 | saved A0-A6 | scheduler (the A0 patch proves the split) |
| +0x6C | 4 | saved USP | scheduler |
| **+0x70** | 4 | **saved PC** (seeded from +0x1E at start) | scheduler, 0x2192 |
| **+0x74** | 2 | **saved SR** | 0x2192 |
| **+0x76** | 4 | **pending events** (bit set) | traps 0x0A/0x0B/0x0C, poster 0x2562 |
| **+0x7A** | 4 | **wait mask** | traps 0x0A/0x0B |
| +0x00 | 4 | link - used as a queue link by trap 0x19's 0x04C2 list | 0x21D6 |
| +0x10 | 4 | second link - the 0x04C6 list | 0x21D6 |

Fields are written up to at least +0x98, so the record is **at least 0x9C bytes**.

---

## 4. The flags word (+0x16)

| Bit | Meaning | Set by | Cleared by |
|---|---|---|---|
| 0 | created | `PosCreateObjectBody` | |
| **1** | **blocked** | waits (0x2BD8/0x2C34), trap 0x19 | **the event poster 0x2562 - the only bit it touches** |
| 2 | runnable / queued | `PosStartProcess` | |
| 3 | waiting, trap 0x0A flavour | 0x2BD8 | |
| 4 | started | `PosStartProcess` | |
| 5 | waiting, trap 0x0B flavour | 0x2C34 | |

**Bits 3 and 5 are markers, not semantics.** `0x2BD8` and `0x2C34` are byte-identical apart from
which of the two they set, and the poster ignores both. Whatever reads them is elsewhere; do not
assume traps 0x0A and 0x0B wake differently.

---

## 5. The three processes

`PosCreateObject` (0x2E98) has exactly three call sites, all at boot:

| Handle | Name | Name at | Priority | Attr bit 15 | Entry | Role |
|---|---|---|---|---|---|---|
| 1 | `FREE` | 0x0C62 | 5 | no | 0x1A96 | idle - what runs when no ready queue is populated |
| 2 | `PRO1` | 0x0C66 | 1 | no | 0x7E0E | `PIUSERMAIN` / `AUTO_START` - the application |
| 3 | `RTC ` | 0x12B0 | 14 | yes | 0x3BFE | real-time clock service, highest priority |

`FREE` and `PRO1` share one 8-byte literal `"FREEPRO1"` at 0x0C62, sliced into two 4-char names.
`"RTC "` sits immediately before the `RT-CLOCK` module directory record at 0x12B4.

The priorities are the whole scheduling story: RTC (14) preempts everything, the application (1)
runs whenever nothing else needs the CPU, and FREE (5) sits between them purely so the priority
scan never falls off the bottom.

---

## 6. What is NOT statically recoverable

The ROM image contains the *code* that builds these structures, not the structures themselves:

- **`DAT_00000664`, the descriptor size, is zero in the image.** It is computed at boot, so the
  real record length cannot be read statically. We know only that fields extend to +0x98.
- **The object table at 0x0A8A is all zeros** in the image - it is populated at run time.
- **The ready-queue heads at 0x0B06 are zero**, likewise.

Anything in this document about *contents* (as opposed to layout) therefore comes from reading the
code that writes them. To confirm actual values, dump a booted card or an emulator run and apply
`/PIOCOS/PiocOsObject` to a descriptor.

---

## Provenance

The descriptor field map, the flags bits, and the two-layer distinction were read from the image.
The three-process table was established earlier and re-confirmed against the creation call sites.
Section 6 lists exactly what static analysis cannot answer, rather than guessing at it.
