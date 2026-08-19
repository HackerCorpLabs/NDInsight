# ND-10130 — Xmessage for SINTRAN-III/VS

> Status: IN-PROGRESS — real J-version floppy decoded, complete load/start scripts recovered

| Field | Value |
|-------|-------|
| ND article number | `ND-10130` |
| Product name | X-Message for SINTRAN III/VS |
| Functional category | Networking & Communications (inter-task messaging) |
| CPU target | ND-100 |
| Related products | `ND-10373` X-Message (Inter System) — see [../ND-10373/README.md](../ND-10373/README.md), the networked/file-transfer-capable sibling of this base single-system product. **Historical note:** this is XMSG version **J** (1985) — an early revision, superseded many times over; the current live revision this repo's XMSG reverse-engineering project (`SINTRAN/XMSG/`) works against is **L03**. This floppy predates that work by years and is unrelated to it beyond sharing the product name. |

## What is known — real floppy, decoded

Floppy `10130J00` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) mounts cleanly:

```
XMSG-IN-S-J00:COM              installer command file (text, not decoded here)
XMSG-01-S-J00:COM, XMSG-02-S-J00:COM   further installer command files
XCOM-L01:PROG                  a compiled command-processor program
XMSG-LOAD-S-J00:MODE           real, decoded — loads the kernel and XROUT onto RT segments (below)
XMSG-START-S-J00:MODE          real, decoded — example startup/configuration script (below)
XMSG-START-S-J00:BATC          real, decoded — example batch job wrapping the above (below)
XMSG-SYSTABS-J00:SYMB, XMSG-POFTABS-J00:SYMB   system/port-table symbol files
XMSG-VALUES-J:SYMB, XMSG-PL-VALUES-J:INCL      constant definitions (SYMB for assembler, INCL for PLANC)
XMSG-LIBRARY-J00:BRF           the programmer call library
XMSG-KE-S-CX-J00:BPUN, XMSG-XR-S-CX-J00:BPUN, XMSG-SY-S-CX-J00:SYMB   kernel/XROUT/symbols, CX CPU variant
XMSG-KE-S-NX-J00:BPUN, XMSG-XR-S-NX-J00:BPUN, XMSG-SY-S-NX-J00:SYMB   kernel/XROUT/symbols, NX CPU variant
XMSG-COMMAND-J00:PROG          the interactive XMSG-COMMAND configuration program
```

## Loading the XMSG kernel — real, decoded `XMSG-LOAD-S-J00:MODE`

Source: byte-for-byte decode (`byte & 0x7F`). [MODE]

```
@CC
@CC   ***************************************************
@CC   **         Load or reload XMSG version J.        **
@CC   ***************************************************
@CC   *
@CC   * 1) The product (BPUN) files must have been installed properly.
@CC   *    If not, read the Product Description Sheet supplied with
@CC   *    the XMSG floppy disks and install XMSG.
@CC   * 2) Unless the XMSG files was copied to user UTILITY, edit this
@CC   *    file accordingly:
@CC   *    Substitute (UTILITY)
@CC   *    with       (Name of user which has the XMSG :BPUN files).
@CC
@CC   * Now you can (re)load XMSG:
@CC   *   1.  Load XMSG-KERNEL onto its segment
@CC   *   2.  Load XMSG-XROUT onto its segment
@CC   *   3.  Create foreground programs XROUT and XTRACE
@CC
@SINTRAN
@STOP-XMSG
@EXIT
@HOLD 0 0
@HOLD 3 2
@UNFIX 33
@RT-LOADER
CLEAR-SEGMENT 33
YES
YES
SET-PAGE-TABLE 2
NEW-SEGMENT 33,2,ND,,,,,
READ-BINARY (UTILITY)XMSG-KERNEL-J:BPUN,,,,
END
CLEAR-SEG 34
YES
YES
SET-PAGE-TABLE 2
NEW-SEG,34,2,DM,,,,
READ-BINARY (UTILITY)XMSG-XROUT-J:BPUN,,,
YES
END
SET-PAGE-TABLE 2
DECLARE-PROGRAM XROUT,,,
CHAN-RT-DESC XROUT,100,34,33,0,,,
DECLARE-PROGRAM XTRACE,,,,
CHAN-RT-DESC XTRACE,100,34,33,4,,,
END
WRITE-SEG 33,,,,,,
WRITE-SEG 34,,,,,,,,,
EXIT
@CC
@CC   * 4.  Patch in IMAGE and RESIDENT to flag XMSG loaded (XMSGU+4)
@CC
@LOO-AT RES
165/-1
.
@LOO-AT IMAGE
165/-1
.
@CC
@CC  And now you can start XMSG by using the SINTRAN-SERVICE command START-XMSG.
@CC
```

**Reading it**: XMSG's kernel and XROUT are loaded as two RT segments (33 and 34) via
`@RT-LOADER`, each declared as a foreground program (`XROUT`, `XTRACE`) with `CHAN-RT-DESC`. A
direct memory patch (`@LOO-AT RES`/`@LOO-AT IMAGE`, offset 165, value -1) flags XMSG as loaded at
a fixed offset from the symbol `XMSGU` — a real, low-level SINTRAN system-table patch, not a
documented command.

## Starting and configuring XMSG — real, decoded `XMSG-START-S-J00:MODE`

Source: byte-for-byte decode, in full. [MODE]

```
@cc
@cc    ********************************************************************
@cc    **  XMSG startup mode file for ND-xxx ***  (XMSG-START-S-J:MODE). **
@cc    ********************************************************************
@cc
@cc    This is an EXAMPLE of what a typical XMSG startup file looks like.
@cc    It contains a superset (in most cases) of what a user would normally
@cc    require. The XMSG startup file would normally be invoked by
@cc    one of the INITIAL-COMMANDs
@cc
@cc    First we start XMSG and wait while the segments get fixed.
@SINTRAN
@STOP-XMSG
@EX
@HOLD 0 0
@HOLD 2 2
@SINTRAN
@START-XMSG
@EX
@HOLD 0 0
@HOLD 3 2
@cc
@cc    We now get the XMSG-COMMAND program to configurate the system.
@cc
@(UTILITY)XMSG-COMMAND
SET-PRI                      Request XMSG to become a privileged task
''
''     Open the trace file and start tracing:
''
'OPEN-TRACE,,,               Open (RT)XMSG-TRACE:DATA (10 pages contiguous)
''                           Start tracing these events:
'ENA-TRA,,8                  User calls to XMSG
'ENA-TRA,,9                  User return from XMSG calls
''
''     Define XMSG dump files.
''     (If XMSG crash, XMSG will be dumped for post-mortem investigation)
''
'DEFINE-DUMP-FILES,,,,       The files (RT)XMSG-SEGMENT-DUMP:DATA,
''                                     (RT)XMSG-TABLES-DUMP:DATA
''                                     (RT)XMSG-BUFFER-DUMP:DATA
''                           are defined as dump files.
''
''     Define XMSG restart file and enable auto-restart of XMSG.
''     (If XMSG crash, the restart file will be appended to the batch queue.)
''     (Note that the output file of type OUT must exist.)
''
'DEFINE-RESTART-FILES (UTI)XMSG-START-S-J:BATC,(SYS)XMSG-START:OUT
'AUTO-RESTART-ON
''
''     We now define symbolic names of our system:
''
'DEF-REMOTE,,DOLE 284
'DEF-REMOTE,,ND-284 284
''
''     See how things are going:
''
'LIST-NAMES,,,,
''
''     Disable the trace events that we do not need at runtime.
''
'DIS-TRA,,8
'DIS-TRA,,9
''
''     Close the trace file and stop tracing:
''
'CLOSE-TRACE,,,
''
''     Terminate XMSG command-program:
''
EXIT
```

**Reading it**: real `XMSG-COMMAND` verbs confirmed — `SET-PRI` (privileged task), `OPEN-TRACE`/
`ENA-TRA`/`DIS-TRA`/`CLOSE-TRACE` (event tracing), `DEFINE-DUMP-FILES` (post-mortem crash dumps),
`DEFINE-RESTART-FILES`/`AUTO-RESTART-ON` (auto-restart on crash, re-queues the batch job named
below), `DEF-REMOTE` (defines a symbolic system name mapped to a numeric node — here `DOLE`/
`ND-284` both mapped to node `284`), `LIST-NAMES`. Lines prefixed `''` are commented-out
explanatory text inside the script; lines prefixed a single `'` are commented-out *commands* left
as a hint for what else is available.

## The batch wrapper — real, decoded `XMSG-START-S-J00:BATC`

```
@ENTER SYSTEM,<password>,<project password>,<maximum time>
@cc  ...
@MAC
%
% Reset output file byte pointer
%
10/ MON 143; COPY DT SD; COPY DA; COPY DD; MON 73; MON 0; MON 74; MON 0; MON 0
10!
@cc    Run the XMSG startup file as a mode job.
@MODE (UTILITY)XMSG-STARTEX-J:MODE,,
@cc Terminate batch file with double ESCAPE:
```

This is exactly the file `DEFINE-RESTART-FILES` above points auto-restart at — confirms the
crash-recovery loop is real: XMSG crash -> restart file re-queued -> `@ENTER SYSTEM` -> runs the
startup `:MODE` job again via `@MODE`.

## Documentation
- Product Information (PI-sheet): [../../Product-Info/ND-10130-A1-EN.md](../../Product-Info/ND-10130-A1-EN.md)

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x` for listing/extraction, `byte & 0x7F` for all three script files).
- **TODO:** `XMSG-IN-S-J00:COM`/`XMSG-01-S-J00:COM`/`XMSG-02-S-J00:COM` (the actual product
  installer command files, as opposed to the load/start scripts above) were not decoded.

---
**Parent:** [../README.md](../README.md) (Software catalog)
