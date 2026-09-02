# MON 60B subfunction 037B - ICSLOAD (LOAD-CONTROL-STORE)

Loads a file into the ND-500 **writable control store (microcode / WCS)**. This is the operator
command `LOAD-CONTROL-STORE <file>` and the microcode-load path that gates ND-500 bring-up (the
`ECSLOAD` "control store must be loaded" condition).

**Status:** dispatch byte-verified; handler logic from the authoritative worker source
`5P-P2-MON60.NPL`. **Handler-body L07 byte-location is pending** - see [Byte status](#byte-status).

- Parent (dispatcher + overlay proof): [`../README.md`](../README.md)
- Full subfunction map: [`../60B-5IFUNC-dispatch-table.md`](../60B-5IFUNC-dispatch-table.md)
- Emulator model: [`60B-037B-ICSLOAD.pseudo.c`](60B-037B-ICSLOAD.pseudo.c)

## Dispatch

```
MON 60B (A -> param list, params[0]=037B)
  -> N500M @ 030416B (050-S3I5PIT, 5PIT context)   [byte-verified]
  -> range-check params[0] <= 177B                  [byte-verified: SAT 177]
  -> A := 5IFUNC[037B] = ICSLOAD ; A =: P           [map 3-way verified; caller call site 006114]
  -> ICSLOAD: copy CS file name (param 5P3, <=200B bytes) user -> MON60 buffer (FRUSMOVE)
  -> GO FAR 5NOPAR -> common system-monitor path -> resident driver loads the WCS from the file
```

## Handler (from `5P-P2-MON60.NPL`, the MON 60 worker source)

```npl
ICSLOAD:     % FUNCTION=037: LOAD CONTROL STORE (LOAD A FILE INTO CS)
       A:=5P3; T:=200; CALL FRUSMOVE   % COPY FILE NAME TO MON60 BUFFER (200B bytes max)
       GO FAR 5NOPAR                    % common path -> system monitor
```

So the N500M-level work for 037B is only: **copy the control-store file name from the caller's
parameter 3 (`5P3`) into the MON60 buffer** (max `200B` = 128 bytes), then hand off. The actual
control-store write happens in the common `5NOPAR` path -> system monitor -> resident ND-500 driver
(**not carved yet** - this is a "more than MON 60" dependency; see parent SCOPE NOTE).

## Parameter / register contract

| item | meaning | verdict |
|------|---------|---------|
| `A` (entry) | address of the parameter list | VERIFIED (dispatcher bytes) |
| `params[0]` | subfunction code = `037B` | VERIFIED |
| `params[3]` (`5P3`) | pointer to the control-store file name string | from NPL source |
| copy length | `200B` bytes (128) max, via `FRUSMOVE` | from NPL source |
| return | skip = success, direct = error (per MON 60B convention) | documented |

## Byte status

- **VERIFIED (L07 bytes):** the dispatch chain to `N500M @ 030416B` and the range check; `5IFUNC[037B]`
  maps to the LOAD-CONTROL-STORE handler (3-way: caller call site `006114`, documented `LDCS 37B`,
  and the NPL `5IFUNC` slot 037).
- **From NPL source (authoritative logic, different revision than L07):** the `ICSLOAD` body
  (`FRUSMOVE` of `5P3`, `200B` max). Not fabricated - it is the worker's own source.
- **PENDING:** locating the `ICSLOAD` body in the L07 bytes. Blocked because `5IFUNC` is `*2BANK`
  (data bank), so the L07 table + handler bodies are not at symbol-anchored addresses in the
  `050-S3I5PIT` code image. Resolving the bank-2 5IFUNC table (parent TODO) will supply the exact
  L07 address for a byte-confirmed `.ASM`.

## Emulator relevance

This is the front half of the **control-store gate** that blocks ND-500 bring-up in the emulator
(the `ECSLOAD` loop that hung `VERSION`). MON 60B/037B tells the resident driver to load microcode
from a named file; the emulator must model the common-path -> driver -> WCS write (and, to satisfy the
gate without real microcode, report the control store as loaded). See
`SINTRAN\ND500\ND500-STATUS-AND-INDEX.md` (control-store section).
