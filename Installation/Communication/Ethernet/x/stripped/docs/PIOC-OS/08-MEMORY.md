# PIOC-OS - Memory management

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: three allocators identified. Heap free-list node format is PARTIAL.

---

## 1. Three separate allocators - do not confuse them

| Allocator | Region | Freed? | Used for |
|---|---|---|---|
| **General heap** | 0x1300-0x1A00 | yes, first-fit free list | process descriptors, port structures, list nodes |
| **PLANC frame arena** | separate, bump-allocated | **never** | routine activation frames |
| **Fixed pools** | statically placed | returned to their own free list | RX buffers, XTRB / COBF / RbRcv / RbCmd / connection records |

The frame arena being non-reclaiming is the single most surprising property of this firmware and is
covered in detail in the `ghidra-planc` skill. It is why a caller can legitimately read results out
of a callee's frame *after* that callee has returned.

---

## 2. The general heap

| Address | Role |
|---|---|
| 0x1300-0x1A00 | heap body |
| 0x0D00-0x0D0A | heap control words |
| `HeapInitFreeList` 0x35BE | build the initial free list |
| `HeapAllocateBlock` 0x36A4 | first-fit allocation |

**The heap top is computed at boot from the board configuration record**: `(0x04CA)+0x30` gives the
DRAM size in 2KB pages, and the heap ceiling is derived from it (`02-BOOT-AND-INIT.md` section 4).
So the usable heap depends on what the ND-100 host declared, not on anything in the ROM. An
emulator that leaves the board config zero will produce a zero-sized or wrongly-sized heap.

Note the control words at 0x0D00-0x0D0A sit immediately below the module directory record at
0x0D0C - adjacent but unrelated structures. Do not read 0x0D00 as part of the directory.

**NOT ESTABLISHED**: the free-list node layout, the allocation granularity, and whether there is a
coalescing free.

---

## 3. The PLANC frame arena

Frames are **bump-allocated from a separate arena and never popped**. Only the pair
`(saved A6, return address)` lives on the CPU stack.

Two allocators exist, and they differ in skip distance - which matters because the bytes after the
call are data, not code:

| Allocator | Skip distance | Inline descriptor |
|---|---|---|
| **0x4492** | **4** | one longword = the frame size. Used by essentially every PLANC routine, including all 27 trap handlers |
| **0x4456** | **8** | 8 bytes. Used by the trap #2 dispatcher at 0x3498 |

There are **three copies of the leaf runtime**, one per linked bank (around 0x134xx, 0x44xx and
0x6BCF0-0x6BF8B), each with its own frame allocator. 0x4478 is byte-identical to 0x13524. Check
which bank a caller sits in before assuming which copy it uses.

Frame layout is documented in the `ghidra-planc` skill; the field that matters for memory purposes
is **+0x00 `STP`, the next-free cursor** - the prologue follows it to reach its own frame and
republishes it for its callee. The arena is therefore a simple stack of frames that only ever grows
during a call chain, unwound wholesale rather than per-frame.

---

## 4. Fixed pools

Each has its own init and free-list routine. Identified so far:

| Pool | Init | Free |
|---|---|---|
| RX buffer pool | `rxpool_init` 0x5512, `maybe_init_buffer_pool_188da` 0x5322 | `drain_freehead_to_readyring` 0x51D0 |
| XTRB | `LNCNInitFreeXtrbPool` 0x8342 | |
| COBF | `LNCNInitFreeCobfPool` 0x83E8 | |
| RbRcv | `LNCNInitFreeRbRcvPool` 0x84EA | `LNCNFreeRbRcv` 0x84BC |
| RbCmd | `LNCNInitFreeRbCmdPool` 0x8586 | `LNCNFreeRbCmd` 0x855A |
| Connection records | `LNCNInitConnPool` 0x8694 | `LNCNClearAndFreeConn` 0x85F4 |
| POCS message buffers | `PocsInitMessageBufferPool` 0xC4AC | |
| POCS session descriptors | `PocsInitSessionDescriptorPool` 0xC7B8 | `PocsReleaseSessionDescriptor` 0xCBCC |
| XGate free list | `XGateFreeListInit` 0xC31E | `XGateFreeListAppendEntry` 0xC2F0 |

### The behaviour that matters for emulation

**RX pool exhaustion is a SILENT drop.** When no free node is available the frame is discarded and
the counter at 0x188A0 is bumped, and **the host is never notified**. An implementation that
reports an error here is being more helpful than the real card and will diverge from it.

---

## 5. The port pools

Covered in `06-IPC-AND-MESSAGING.md`, repeated here because they are allocators: two port pools at
**0x2D472** (classes 0 and 1) and **0x2D4F4** (class 2), plus the name registry at **0x2D354**.
All three carry a **word `0xAAAA` at offset +4** as an "initialised" signature; a mismatch makes the
port calls return -2.

---

## 6. Open

- Heap free-list node format, granularity, and whether frees coalesce
- Sizes of every fixed pool (element count x element size)
- Where the frame arena physically lives and how large it is
- Whether the arena is ever reset other than by unwinding

---

## Provenance

The allocator inventory and the two frame allocators are established. The heap-top dependency on
board config is a prior finding. Section 6 is what static reading has not answered.
