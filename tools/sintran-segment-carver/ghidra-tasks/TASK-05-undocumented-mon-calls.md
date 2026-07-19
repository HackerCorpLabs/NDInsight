# TASK-05 — Verify undocumented / unclear monitor calls

Origin: a request to clarify SINTRAN MON calls that are undocumented or poorly
documented, by inspecting the actual implementation. English translation of the
request is in §A. **Read this whole file — the extraction method differs from the
other tasks (see §D).**

## A. The request (translated from Norwegian)

> I have a small list of MON calls that are not officially documented — some I
> have a handle on, others are unclear. Some are carry-overs from the NORD-1 era
> (pre-SINTRAN, e.g. MON13, MON14, MON15 from TSS) that exist only because of old
> code. The first TSS call I'm really only guessing at is **MON42**, and I'd like
> to verify it. **MON45 (DBRK)** is another. **MON51** is the SINTRAN version of
> DBRK, and I have no firm documentation.
>
> My ND-500 TODO (all > 0377, so not in the ND-100 — the ND-500 monitor must be
> strongly involved):
> - **MON 410, 411, 416, 417, 425, 426, 427, 500, 501, 505** (fixseg, unfix,
>   wsegn, mxpisg, sprname, gprnum, gprname, startpr, stoppr, gerrcod — some have
>   clear names, others not).
> - **MON 510.**
> - **MON 511 (DVIO)** — a combination of the documented DVINST/DVOUTS (503/504);
>   I still haven't fully verified how the parameters are set.
> - **MON 512** — XMSG for ND-500 (I noted it somewhere). No documentation.
> - **MON 513** — used by convert-domain-a03; can be called with 1..6 parameters.
>   Not figured out.
> - **MON 515 (5MTRANS)** — async disk transfer, check event, start process. ND
>   internal.
> - 511, 512, 513 are still showstoppers. But these are 500-calls and probably not
>   directly coded in the ND-100 listing.
>
> **MON45** is GTYPR (GetTypeRing) in the ND-500, but DBRK in the ND-100 (above).
> I've concluded GTYPR works like **MON327 function 4**.
>
> Actual ND-100 calls that ARE documented but I'm unsure about:
> - **MON5 RDISK** / **MON6 WDISK** (ReadScratchFile / WriteScratchFile) — unsure
>   exactly how they should behave.
> - **MON67 (OSIZE / OutBufferSpace).**
> - **MON74 (SETBT / SetStartByte)** — unsure for random-access files.
> - **MON75 (REABT / GetStartByte)** — doc says "mass-storage files only", but some
>   programs call it for file number 0 = the SINTRAN command buffer. Possibly.
> - **MON120 (WFILE / WriteToFile)** — some old programs use it as a seek(0) by
>   setting block=0 bytes=0 on a file opened for read. Question: can the same
>   technique seek to somewhere other than offset 0?
> - **MON144 (MAGTP / DeviceFunction)** — a monster that gained functionality every
>   SINTRAN version. Documented, but I'd love to see the implementation.
> - **MON304 (SIBAS)** — not that interested (nothing I use touches SIBAS).
> - **MON313 (IBRSIZ / InBufferState)** — is it only allowed for devices, never
>   files?
> - **MON327 (FSMTY / FileSystemFunction)** — FORTRAN-100-F (but not -G02) uses it
>   with function 2 to find the number of bytes in the SINTRAN command buffer.
>   Probably old FTN does too. Monster function #2 (MON144 is #1) — would be good
>   to see the code.

## B. ND-100 calls (< 0400) — documented status + what to verify

The definitions below come from the repo MON docs
(`../../../Developer/MON/calls/*.yaml`, `../../../Developer/MON/Monitor Calls.md`).

| MON | Name | Documented? | What to VERIFY in the code |
|-----|------|-------------|----------------------------|
| 5 (RDISK) | ReadScratchFile | yes (`5B_ReadScratchFile.yaml`) | block-number → byte-offset math; scratch file = file 100B; block size (SetBlockSize) handling |
| 6 (WDISK) | WriteScratchFile | yes (`6B_WRITESCRATCHFILE.yaml`) | mirror of RDISK; write path |
| 13,14,15 | ClearInBuffer/ClearOutBuffer/(TSS) | 13,14 yes; 15 unclear | confirm they are TSS carry-overs; what 15 does (no YAML) |
| 42 | (TSS, guessed) | **NO** | identify the handler and semantics |
| 45 (DBRK) | DefineBreakpoint (ND-100) / GTYPR (ND-500) | partial (`Monitor Calls.md`: DBRK 45B) | ND-100 DBRK behavior; confirm ND-500 GTYPR ≡ MON327 function 4 |
| 51 | DBRK (SINTRAN version) | **NO firm doc** | how it differs from MON45 |
| 67 (OSIZE) | OutBufferSpace | yes | exact returned value (bytes free vs used) |
| 74 (SETBT) | SetStartByte | yes | behavior for **random-access** files |
| 75 (REABT) | GetStartByte | yes ("mass storage only") | does it work for **file 0** (command buffer)? |
| 120 (WFILE) | WriteToFile | yes | the block=0/bytes=0 **seek** trick — can it seek to offsets other than 0? |
| 144 (MAGTP) | DeviceFunction | yes (monster) | full function-code table (grew per version) — dump the dispatch |
| 304 (MAPS1B) | SendSIBASMessage | yes | (low priority) |
| 313 (IBRSIZ) | InBufferState | yes | devices-only, or files too? |
| 327 (FSMTY) | FileSystemFunction | yes (monster #2) | full function-code table; function 2 = bytes in command buffer; function 4 = GTYPR? |

## C. ND-500 calls (> 0377) — go to the ND-500 monitor

These are NOT in the ND-100 dispatch (GOTAB only indexes 0..255). They are handled
by the **ND-500 System Monitor** — see [TASK-02](TASK-02-nd500-system-monitor.md)
(`030-S3SM5.bin`, base `0x4000`, `N500-SYMBOLS`).

| MON | Name (friend's note) | YAML present? |
|-----|----------------------|---------------|
| 410 | FIXINMEMORY (fixseg) | yes `410B_FIXINMEMORY.yaml` |
| 411 | MemoryUnfix (unfix) | yes |
| 416 | SaveND500Segment (wsegn) | yes |
| 417 | MaxPagesInMemory (mxpisg) | yes |
| 425 | SetProcessName (sprname) | yes |
| 426 | GetProcessNo (gprnum) | yes |
| 427 | GetOwnProcessInfo (gprname) | yes |
| 500 | StartProcess (startpr) | yes |
| 501 | StopProcess (stoppr) | yes |
| 505 | GetTrapReason (gerrcod) | yes |
| 510 | ? | **NO** |
| 511 | **DVIO** (combines DVINST 503 / DVOUTS 504) | **NO** — verify parameter passing |
| 512 | **XMSG for ND-500** | **NO** |
| 513 | used by convert-domain-a03, 1..6 params | **NO** |
| 515 | **5MTRANS** async disk transfer / check event / start process | partial (SMTRANS, `Monitor Calls.md` §10.6.2) |

Priorities: **511, 512, 513** are the showstoppers.

## D. Extraction method — IMPORTANT (why this is not a plain Ghidra load)

The ND-100 MON handler routines (`RDISK`=072021... wait, `RDISK`=**102021**,
`WFILE`=102132, `MAGTP`=114707, `OSIZE`=111254, `SETBT`=112200, `REABT`=104005 —
all `SYMBOL-1-LIST`, i.e. resident kernel) do **not** sit cleanly in any single
carved `.bin`. Their addresses fall in the paged **level-4 / resident composite
address space** that only exists assembled at runtime; a scan of all carved
segments finds different (mostly unrelated) content at those addresses. So loading
one segment in Ghidra at a fixed base will NOT show the real handler.

Two ways to get the real code:

1. **Live DAP recovery (recommended for the ND-100 <0400 handlers).** Boot SINTRAN
   under nd100x, set an instruction breakpoint at the handler's symbol address
   (e.g. `RDISK` = octal 102021 = `0x8411`), invoke that MON (drive a program /
   command that calls it), and dump/disassemble at the break — the correct paged
   view is then mapped. This is the same technique used to recover the semaphore
   and MON-dispatch code. It runs in WSL (DAP), not Windows Ghidra.
   - Handler symbol addresses (octal, from `SYMBOL-1-LIST` L07): `RDISK=102021`,
     `WDISK=102023`, `WFILE=102132`, `OSIZE=111254`, `SETBT=112200`,
     `REABT=104005`/`112250`, `MAGTP=114707`/`026354`, `GTYPR=107550`/`113312`.
     (Two values = the routine appears in two mapped contexts.)
   - To find a handler with no name symbol (MON42, MON51): read `GOTAB` (octal
     071233 in `116-S3SERWD.bin`, base `0x600`) at index = the MON number; the
     word there is the handler address. Then breakpoint it live.

2. **ND-500 monitor in Ghidra (for the >0377 calls).** Do [TASK-02](TASK-02-nd500-system-monitor.md)
   on `030-S3SM5.bin` and correlate the extended-MON handlers (511/512/513/515
   especially) with `N500-SYMBOLS`.

## E. Deliverable
Write `versions/L-VSX-500/re/TASK-05-results.md`: for each MON call, the verified
semantics (handler address hex+oct, parameter registers/stack layout, function-code
tables for MAGTP 144 and FSMTY 327), and a yes/no on each of the friend's specific
questions (§A). Mark VERIFIED vs UNCERTAIN. This can then update the repo MON YAMLs
and `Developer/MON`.
