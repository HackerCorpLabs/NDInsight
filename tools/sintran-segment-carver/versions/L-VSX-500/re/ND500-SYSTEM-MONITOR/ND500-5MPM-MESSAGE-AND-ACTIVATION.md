# 5MPM message build + activation (ND-100 -> ND-500 signaling)

How the SINTRAN system monitor sends an operation to the ND-500: write a message to the mailbox,
point the interface MAR at it, poke CONTROL to activate. Ties the byte-validated IOX register map
(`ND500-3022-IOX-INTERFACE.md`) to the activation protocol in
`SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md` sections 5-6 (byte-verified from NPL).

## The activation sequence (VERIFIED - bus doc 5.2 `ACT50`, NPL MP-P2-N500.NPL:3052-3099)
```
ACT50: A:=5MBBANK; T:=HDEV+LMAR5 ; *IOXT    ; MAR (MS) := message bank      (LMAR5 = dev+1)
       A:=X       ; *IOXT                    ; MAR (LS) := message address
       A:=5       ; T:=HDEV+LCON5 ; *IOXT    ; CONTROL := 5 (int-enable + activate/lock)  (LCON5 = dev+5)
```
`LCON5=dev+5` and `LMAR5=dev+1` are the byte-validated registers. Writing CONTROL bit 2 locks the
interface and starts the ND-500; the microcode DMA-fetches the message the 24-bit MAR points at.

**Preconditions the driver checks first (bus doc 5.1):** read `RSTA5` (STATUS, dev+2); if `5CLOST`
(clock stopped, bit 9) -> give up (control store not loaded -> the gate, see
`ND500-CONTROL-STORE-GATE.md`); if `5ILOCK` (bit 5, still running) -> terminate first, then activate.

## The mailbox message (VERIFIED - bus doc 6.2, symbols)
Message lives in ND-100 physical memory, bank `5MBBANK`; accessed with the physical primitives
`LDATX`/`STATX`/`LDDTX`/`STDTX` (the `143300B` region in `030-S3SM5`). The interface reaches the same
memory via its 24-bit MAR.

Header (offsets octal; these match the swapper `N500-SYMBOLS.SYMB` values I read earlier):
| off | symbol | content |
|-----|--------|---------|
| -1 | `5MSFL` | message flag word (5IEXQUEUE / 5CPUBOUND / 5IBRK ...) |
| 0-1 | `LINK` | queue link (double word) |
| 2 | `N5STA` | message STATUS word |
| 3 | `SENDE` | sender (watchdog = -1) |
| 4 | `X5CPU` | receiver / CPU field (checked = MPACTIVE) |
| 5 | `X5ACT` | size / activation field |
| 6 | `MICFU` | **micro-function (the command code the ND-500 executes)** |
| 7 | `N500A` | ND-500 logical address |
| 11 | `STOPR` / `N100A` / `ACPRO` | stop reason / ND-100 phys addr / actual process (**direction-dependent overlay**) |
| 13 | `NRBYT` / `MCNO` | byte count / mon-call number (direction-dependent) |
| 37 | `SMCNO` | saved mon-call number |
| 143 | `SPFLA` | special flag -> DECOMESS dispatch override |
| 144 | `XADPR` | process descriptor address |

**Reconciliation note:** offset `13` is `NRBYT` (byte count) or `MCNO` (mon-call number) depending on
direction - the message block overlays fields per direction (bus doc flags offset 11 the same way).
This reconciles the earlier "`MCNO=13`" (from the swapper symbols) with the bus doc's "`NRBYT=13`" -
same slot, direction-dependent meaning. Not a contradiction.

## What the ND-500 does (bus doc 5.4, DERIVED from ND-05.012.01 sec 13)
On activate, microcode leaves IDLE, DMA-fetches the message, sets status "in process", executes
`MICFU`, writes the answer status + result fields, raises the ND-100 **level-12** interrupt (STATUS
"finished" + interrupt if CONTROL bit 0 set).

## Return path (level-12 ISR) - NEXT byte-carve
`5STDRIV` (level-12 ISR) reads STATUS, walks the exec-queue from `MAILINK`, and dispatches on each
message's `N5STA`/`STOPR` (`CHN5STATUS` -> `DECOMESS` -> `MCHANDEL`). NPL-documented
(`MP-P2-N500.NPL:659,730-759,803-818,1251-1406`); locating it in `030-S3SM5`/`026-S3IMPIT` bytes is
the remaining Phase-1 carve.

## Emulator model (complete enough to implement the interface)
1. Guest writes the message to mailbox RAM (bank `5MBBANK`) via normal memory writes.
2. Guest writes `LMAR5` (dev+1) x2 = message address, then `LCON5` (dev+5) = 5 (activate).
3. **Emulated ND-500 side:** on `LCON5` activate, read the message at MAR, execute `MICFU`
   (dispatch via the ND-500-side op - or, for the interface-only goal, just produce the answer),
   write answer status + `N5STA`, and raise **level-12** on the ND-100.
4. Guest's `5STDRIV` ISR reads `RSTA5`, walks the queue, dispatches on `N5STA`/`STOPR`.
5. Keep `RSTA5` STATUS bit 9 (`5CLOST`) CLEAR so the control-store gate passes (see gate doc).

VERIFIED: register map, STATUS gate, activation register usage (from S3SM5 bytes) + activation
sequence & message layout (from bus doc / NPL). INFERRED/DERIVED: exact microcode steps (not in
SINTRAN sources); the `5STDRIV` byte location (NEXT).
