# The ND-500 Linkage-Loader (NLL) - what it is, what it makes, and why we need it

**Purpose of this page:** one place that ties together everything this repo knows about NLL.
Every claim below is sourced; nothing here is new research. Written 2026-08-18.

---

## 1. What it is

NLL ("ND Linkage-Loader", product **ND-210319**) is the tool that turns compiler output into
runnable ND-500 programs. The vendor manual defines it directly:

> "NLL is a subsystem able to convert relocatable object files in ND Relocatable Format
> (NRF), created by language subsystems, to independent executable programs, or processes."
> - [ND-60.136.04A ND-500 Loader Monitor](../../Reference-Manuals/ND-60.136.04A%20ND-500%20Loader%20Monitor.md)

On the ND-500 there is no "executable file" the way the ND-100 has `:PROG`. A program is a
**domain** - an addressing space of up to 32 **segments** - and a domain is not a file but an
entry in a per-user index file, `DESCRIPTION-FILE:DESC`. NLL is the program that creates and
maintains all of that:

- it reads `:NRF` files from the compilers (FORTRAN-500, PLANC-500, NC, COBOL-500, ...),
- it builds each segment as three SINTRAN files: `:PSEG` (instructions), `:DSEG` (data) and
  `:LINK` (label names, values and debug info),
- it records the domain and its segments in `DESCRIPTION-FILE:DESC`,
- and it resolves references between segments and domains (`LINK-SEGMENT`,
  `LIBRARY-SEGMENT-LINK`, common blocks, entry lists).

NLL itself is an ND-500 program: the domain `LINKAGE-LOAD-H02` (in our H02 copy), started
from SINTRAN with `@ND-500-LINKAGE-LOADER` or from the monitor with `N500: LINKAGE-LOADER`.

## 2. Why we need it

**Every path onto the ND-500 side goes through the domain machinery NLL owns.**

1. **Building anything.** Compile with any ND-500 compiler and you get an `:NRF` file - not
   a program. Only NLL can load that into a domain you can run. The normal loop is three
   commands (see the [operations guide](../../Reference-Manuals/500/ND-500-MON-SETUP-AND-OPERATIONS-GUIDE.md),
   section 3.0):

   ```
   NLL: SET-DOMAIN "MY-DOMAIN"     double quotes = create NEW domain
   NLL: LOAD-SEGMENT TESTPROG      load compiler output (:NRF file)
   NLL: EXIT                       link libraries, update files, return
   ```

2. **Installing vendor products.** The ND-500 product floppies in our corpus (COBOL-500,
   FORTRAN-500, LED, the RG family, NLL itself) ship as `:PSEG`/`:DSEG`/`:LINK` files plus a
   `DESCRIPTION-FILE:DESC` - the exact format NLL owns. Getting them onto a system means
   getting them into the user's description file. On our SINTRAN L image the ND-500 compiler
   chain could not be installed until NLL was in place - the NLL install was the gating step
   of the whole ND-5000 bring-up
   ([NLL-INSTALL-ROOT-CAUSE-PLAN-2026-07-30](../ND5000/NLL-INSTALL-ROOT-CAUSE-PLAN-2026-07-30.md),
   [NLL-INSTALL-SWAPFILE-UNBLOCKED-5SWAP-PROTECT-VIOLATION-2026-07-31](../ND5000/NLL-INSTALL-SWAPFILE-UNBLOCKED-5SWAP-PROTECT-VIOLATION-2026-07-31.md)).

3. **Debugging.** The `:LINK` file NLL writes is what the symbolic debugger reads to show
   names instead of addresses. No NLL, no symbols.

A system **without** NLL is recognizable by its symptoms - documented from a live session in
[Installing the ND-500 Linkage-Loader and Backup System](../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md):
`N500: LINKAGE-LOADER` answers `DESCRIPTION FILE ERROR: DESCRIPTION-FILE / NO SUCH FILE NAME`
(the honest error), while `ND-500-LINKAGE-LOADER` answers the misleading
`TOO LONG PARAMETER` (21 characters treated as a domain name, limit 16).

## 3. What it is NOT

- **Not part of base SINTRAN.** It is a separate product installed from its own floppy
  (`210319H02-XX-01D` in our archive, floppy image `ND-disk-00042.img`),
  and it hard-requires the **Backup System** (210337) to be installed first.
- **Not the same as LINKER-B01.** `LINKER-B01` (see
  [ND500-APPS/LINKER-B01/userguide.md](../ND500-APPS/LINKER-B01/userguide.md)) is the later
  linkage editor that works on self-contained `:DOM` files - the newer domain format that
  replaced the `:PSEG`/`:DSEG`/`:LINK` trio. `CONVERT-DOM` converts the old format to `:DOM`
  (and does NOT read the DESC size fields - it asks the file system, see
  [desc-format.json](../File-Formats/desc-format.json) nonWitnesses).
- **Not the debug monitor.** `MON-DEBUG:PROG` (nd-500-mon-j04) contains the LOOK-AT/debugger
  and PLACE machinery but none of the loader commands, and it never opens a `:LINK` file -
  verified byte-level 2026-08-17
  ([CARVE-ANSWER-FOUR-OPEN-QUESTIONS-2026-08-17](nd-500-mon/CARVE-ANSWER-FOUR-OPEN-QUESTIONS-2026-08-17.md), Q3).

## 4. Its files - all decoded in this repo

| File | Contents | Format doc |
|---|---|---|
| `DESCRIPTION-FILE:DESC` | Per-user index: domain entries (56 bytes) and segment entries (192 bytes), fully byte-mapped from the monitor's own print code | [DESCRIPTION-FILE-FORMAT.md](../File-Formats/DESCRIPTION-FILE-FORMAT.md), [desc-format.json](../File-Formats/desc-format.json) |
| `<segment>:PSEG` | Instruction segment, raw image; size rule `PLB + PSIZE + 1 = file size` | same docs, section 5 |
| `<segment>:DSEG` | Data segment; file maps from logical address 0, `DLB + DSIZE + 1 = file size` | same docs |
| `<segment>:LINK` | NLL's loader table dumped at CLOSE-SEGMENT: 32-byte label records in ascending value order; the universal `32k+1` size is the SMAX off-by-one | [LINK-FILE-FORMAT.md](../File-Formats/LINK-FILE-FORMAT.md), [link-format.json](../File-Formats/link-format.json) |
| `<file>:NRF` | Its input - relocatable output of the compilers | [NRF-FILE-FORMAT.md](../File-Formats/NRF-FILE-FORMAT.md), [nrf-format.json](../File-Formats/nrf-format.json) |

## 5. Command surface (H02)

91 commands, enumerated from the binary's own dispatch table (descriptor table at DSEG
`0x1368`, handler table at `0x4A0C`). The families:

- **Domain/segment lifecycle:** SET-DOMAIN, OPEN-SEGMENT, APPEND-SEGMENT, CLOSE-SEGMENT,
  END-DOMAIN, DEFINE-SEGMENT-SIZE, DELETE/CLEAR/RENAME/COPY/RELEASE-DOMAIN,
  DELETE/CLEAR/RENAME-SEGMENT, LIST-DOMAIN, LIST-SEGMENT.
- **Loading:** LOAD-SEGMENT, TOTAL/LIBRARY/OMITTED/SELECTED-SEGMENT-LOAD, RELOAD-SEGMENT,
  SET-AUTO-LOAD-FILE, RUN.
- **Linking:** LINK-SEGMENT, LIBRARY-SEGMENT-LINK, FORCE-SEGMENT-LINK,
  SET-AUTO-LINK-SEGMENT, COMMON-SEGMENT-OPEN/APPEND/CLOSE/NUMBER, MATCH-RTCOMMON,
  MATCH-COMMON-RT-SEGMENT, LINK-RT-PROGRAM, DEFINE-COMMON, DEFINE-ENTRY,
  PROGRAM/DATA-REFERENCE.
- **Symbol control:** KILL-ENTRIES, GLOBAL-ENTRIES, VALUE-ENTRIES, SYSTEM-ENTRIES-ON,
  LIST-ENTRIES-DEFINED/UNDEFINED, LIST-MAP (these decide what survives onto the `:LINK`
  file).
- **Memory placement:** LOW/HIGH-ADDRESS, SET-SEGMENT-LIMITS, FIX-SEGMENT-
  SCATTERED/CONTIGUOUS/ABSOLUTE, SET-SEGMENT-NUMBER.
- **Traps:** LOCAL/SYSTEM-TRAP-ENABLE/DISABLE, ENTRY-ROUTINES.
- **NRF library maintenance:** NEW/DELETE/APPEND/FETCH-NRF-MODULES, LIST-NRF-ENTRIES,
  LIST-NRF-CODE, WRITE-NRF-EOF-AFTER-MODULE, INSERT-NRF-MESSAGE, PREPARE-NRF-LIBRARY-FILE.
- **Modes/misc:** COMPUTER-MODE (100/500/pioc - NLL also loads ND-100 and PIOC targets),
  LIST-MODE, CHECK-SYNTAX-MODE, PAGE-MODE, LIST-OCTAL, LIST-SYMBOLIC, OUTPUT-FILE, HELP,
  EXIT, RESET, WRITE-DOMAIN/SEGMENT-STATUS, SET-MONITOR-NAME, SET-IO-BUFFERS,
  SUPPRESS-DEBUG-INFORMATION, ABORT-BATCH-ON-ERROR.

## 6. Install and operation - the repo's own guides

- **Install walkthrough (live session, with pitfalls):**
  [INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md](../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
- **Install procedure as a worked example of any ND-500 product install:**
  [ND500-PROGRAM-INSTALL-LINKAGE-LOADER-210319.md](../../Installation/ND500-PROGRAM-INSTALL-LINKAGE-LOADER-210319.md)
- **Concepts + everyday use (domains, segments, the 3-command loop, multi-segment builds):**
  [ND-500-MON Setup and Operations Guide](../../Reference-Manuals/500/ND-500-MON-SETUP-AND-OPERATIONS-GUIDE.md), section 3
- **Vendor manual:** [ND-60.136.04A ND-500 Loader Monitor](../../Reference-Manuals/ND-60.136.04A%20ND-500%20Loader%20Monitor.md)
  (J-era, when the loader and monitor were one book; the commands match)
- **ND-5000 bring-up history (why NLL was the gate):**
  [NLL-INSTALL-ROOT-CAUSE-PLAN-2026-07-30.md](../ND5000/NLL-INSTALL-ROOT-CAUSE-PLAN-2026-07-30.md)

## 7. Reverse-engineering assets in this repo

We hold NLL H02 itself and have started carving it:

| Asset | Path |
|---|---|
| NLL program segment (ND-500 code, 123,989 bytes) | `SINTRAN/ND500/nll-re/LINKAGE-LOAD-H02.PSEG` |
| Utilities note (7-bit text) | `SINTRAN/ND500/nll-re/LINKAGE-LOAD-H02.UTIL` |
| NLL data segment (2,184,977 bytes: command tables, strings, error texts) | NOT committed - re-extract from the floppy image, see [nll-re/README.md](nll-re/README.md) |
| Full disassembly, 31,747 lines (nd500-dis, base 0xB0000000 = segment 22) | NOT committed - regenerates exactly from the PSEG, command in [nll-re/README.md](nll-re/README.md) |
| Carved: the `:LINK` serializer, CLOSE-SEGMENT chain, MON-call wrappers | [CARVE-ANSWER-FOUR-OPEN-QUESTIONS-2026-08-17.md](nd-500-mon/CARVE-ANSWER-FOUR-OPEN-QUESTIONS-2026-08-17.md), Q3 |

Tooling note: ND-500 code is carvable from a Windows session even without a Ghidra
processor - `nd500-dis` from the `pcc-nd500` tree (a WSL binary, callable from Windows as
`wsl <path-to-pcc-nd500>/bin/nd500-dis`; the tree is indexed in the nd500x repository's
`docs/EXTERNAL-ARTIFACTS.md`) handles raw PSEGs and annotates monitor calls inline.

## 8. Open questions

- The L-era `:LINK` variant (SL202-FO-L27) has undecoded string/module regions - needs an
  L-series NLL binary ([LINK-FILE-FORMAT.md](../File-Formats/LINK-FILE-FORMAT.md) section 6).
- Write-side proof of the DESC `size-1` encoding must come from NLL's DESC writer (the
  read side and the file bytes are proven; [desc-format.json](../File-Formats/desc-format.json)
  openQuestions).
