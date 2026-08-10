# PIOC-OS - Kernel API reference (trap #2)

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: all 27 services identified. Argument-block layouts are complete for some services and
partial for others - each entry says which.

---

## 1. The ABI

```
D0 = function code (0 .. 0x1A)
A0 = pointer to the argument block
trap #2
```

Supervisor entry is `PiocOsTrap2Dispatch` @ **0x3498**, reached ONLY through 68000 exception
vector 34 (0x088). Vector 32 (TRAP #0, 0x080) points at the same handler.

**This routine is invisible to every static sweep.** Nothing falls through to it and nothing
branches to it - fallthrough, direct-branch and prologue searches all miss it. It is reachable
only via the vector table.

### Entry sequence

1. `ori #0x400,SR` - raise the interrupt mask
2. **Fast paths for `D0 == 9` and `D0 == 0x1B`, taken BEFORE the table is consulted.**
   `D0 == 9` jumps to 0x25B0. `D0 == 0x1B` has no table entry at all.
3. Save D0-A6 and USP into the current process descriptor at `(0x0650) + 0x30`
4. Switch to the kernel stack at `0x0666 + 0x3FE`
5. Allocate a PLANC frame via 0x4456 - **skip distance 8**, so the 8 bytes at 0x3504 are an inline
   frame descriptor, not code
6. Bounds-check D0 to 0..0x1A and dispatch through `tbl_piocOsTrap2FunctionDispatch` @ **0x0C6A**

### Argument-block versioning

Many argument blocks begin with a **version word** that must match, or the service returns -1
without touching anything else:

- most services require **1**
- **service 0x18 requires 2**

This is a cheap conformance check for an emulator: sending version 0 or 3 must produce -1.

### A reading trap

Every handler begins:

```
4E B9 00 00 44 92    jsr 0x4492          the PLANC frame allocator, skip distance 4
00 00 00 NN          dc.l N              INLINE FRAME DESCRIPTOR - N is the FRAME SIZE
```

Ghidra renders those four bytes as `ori.b #N,D0b`. **They are data.** This misreading already
produced one wrong conclusion in earlier analysis (service 0x19 was recorded as "raises #PRERR
0x16"; 0x16 was its frame size).

---

## 2. The services

| Code | Handler | Service | Argument block |
|---|---|---|---|
| 0x00 | 0x2E72 | Resolve + validate the current process (via 0x0650, checked by 0x2A06) | partial |
| 0x01 | 0x2DEC | Composite: claim slot (0x2340) then slot-op (0x24AA) | `{word version=1, ...}` |
| 0x02 | 0x2D94 | Wrapper around service 0x14 | `{word version=1, ...}` |
| 0x03 | 0x2E98 | **Create object/process.** Name looked up via 0x28E6 | see 03-PROCESS-MODEL |
| 0x04 | 0x3046 | **Start process** - queue runnable, raise reschedule if higher priority | `{word handle}` |
| 0x05 | 0x30F2 | **Delete object** - resolve handle, 0x3840, then CLEAR the object-table slot at 0x0A8A[handle] | `{word handle}` |
| 0x06 | 0x3176 | Handle existence check - resolve + validate, returns 1 | `{word handle}` |
| 0x07 | 0x31D4 | **Look up object by name -> handle.** Builds an 8-byte array descriptor `{origo=arg+2, lower=0, upper=3}` over the 4-char name and calls 0x28E6 | `{word handleOut, char name[4]}` |
| 0x08 | 0x3218 | **Get object name by handle** - descriptor over `desc+0x18`. Inverse of 0x07 | `{word handle, char nameOut[4]}` |
| 0x09 | 0x3476 | **Post / signal an event.** FAST-PATHED at 0x349C -> 0x25B0 before the table. The table slot is the slow path and runs the FREE idle entry 0x1A96. Issued per armed ring slot by the vector-78 handler at 0x7726 | `{word handle, long eventMask}` |
| 0x0A | 0x3286 | **Wait for event (mask).** Sets flags **bit 3** | `{word ?, long mask at +4}` |
| 0x0B | 0x32CA | **Wait for event, second flavour.** Byte-identical to 0x0A except it sets flags **bit 5** | as 0x0A |
| 0x0C | 0x330E | **Atomic read-and-clear of pending events** (`desc+0x76`), at SR=0x2400 | `{long eventsOut}` |
| 0x0D | 0x3EA6 | **Arm a timer.** Snapshots tick 0x0FC2->0x0FC6, resolves a handle, requires `arg+0x0E` in {1,3}, range-checks `arg+0x0A <= 0xA3D70A` | `{word handle, ..., long time@+0x0A, word mode@+0x0E}` |
| 0x0E | 0x3FBE | **Cancel timer(s).** Walks the list at 0x0FCE matching `elem+4 == arg[0]` AND `elem+6 == arg+2`; **`arg+2 == 0` is a WILDCARD**. Returns **-6** if nothing matched | `{word id, long key}` |
| 0x0F | 0x3396 | **Install a 68000 exception vector.** Vector number validated **8..255**, handler longword written to `vectorTable[vec*4]`. This is how vector 78 gets hooked | `{word vectorNum, long handler}` |
| 0x10 | 0x33EA | **Install a callback** at global 0x050E - a function pointer tail-jumped at 0x1EEA in the fault path | `{long handler}` |
| 0x11 | 0x31A8 | **Get own process handle** (`currentProc+0x0E`) | `{word handleOut}` |
| 0x12 | 0x2340 | **Claim slot ownership**, slot 0..7 in the tables at 0x0B66/0x0BE8. **-0x16** out of range, **-0x15** already owned | `{word slot}` |
| 0x13 | 0x23CA | **Release slot ownership** | `{word slot}` |
| 0x14 | 0x2424 | Slot table lookup - indexes the WORD array at **0x0B96** by `arg[0]` | `{word slot}` |
| 0x15 | 0x24AA | Slot operation | partial |
| 0x16 | 0x40A2 | **UNIMPLEMENTED** - returns **-0x0B** immediately | none |
| 0x17 | 0x1F08 | **UNIMPLEMENTED** - returns **-0x0B**, byte-identical to 0x16 | none |
| 0x18 | 0x403A | Timer family - **requires version word 2**, then calls 0x3E00 | `{word version=2, ...}` |
| 0x19 | 0x3348 | **Block self on a host request.** Masks to SR=0x2400, sets reschedule byte 0x0660, sets flags **bit 1**, then 0x21D6: link onto two queues (heads 0x04C6 via `desc+0x10`, 0x04C2 via `desc+0x00`) and **write 1 to the SCIP doorbell 0x00EF0080**, raising the ND-100 interrupt | partial |
| 0x1A | 0x3150 | **Terminate process** (self) | none |
| 0x1B | — | Fast path at dispatch entry, **no table entry** | — |

A 28th longword follows the table holding the same value as 0x09's slot.

### Two corrections to earlier analysis

1. **0x19 is not unimplemented.** The `#PRERR 0x16` reading was the inline frame descriptor (see
   section 1). The genuinely unimplemented services are **0x16 and 0x17**.
2. **0x0D was previously labelled "wait / await event".** Its body is timer work and it pairs with
   0x0E (cancel), so "arm timer" fits the evidence better - but a wait-with-timeout would look
   similar from outside. **Not settled.**

---

## 3. Error codes seen at this layer

| Code | Meaning |
|---|---|
| -1 | argument-block version mismatch |
| -6 | timer cancel found no match |
| -0x0B (-11) | service not implemented (0x16, 0x17) |
| -0x15 (-21) | slot already owned |
| -0x16 (-22) | slot number out of range |

---

## 4. Object lifecycle, as a whole

The services form a complete and symmetric set:

```
create 0x03  ->  start 0x04  ->  [running]  ->  terminate-self 0x1A
                                            ->  delete 0x05   (frees the 0x0A8A slot)

lookup-by-name 0x07  <->  name-from-handle 0x08
own handle     0x11
exists?        0x06
```

**0x05 and 0x1A are different operations** and were previously conflated. 0x1A ends the calling
process; 0x05 releases another object's table slot.

---

## Provenance

All 27 entries were read from `tbl_piocOsTrap2FunctionDispatch` and their handlers on 2026-07-26.
Five entries (0x0F, 0x11, 0x12, 0x13, 0x1A) had been identified independently beforehand and
matched exactly, which is what validates the rest of the read. Services marked "partial" have had
their behaviour established but not their full argument-block layout.
