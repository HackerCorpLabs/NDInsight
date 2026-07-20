# Domain-Handling Test Command Sequence (ND-500 3022 Bus / ND-5000 Octobus)

**Purpose**: the ordered SINTRAN/monitor command script for RetroCore unit tests (boot-harness style) that exercises the ND-100<->ND-500 3022 bus interface or the ND-100<->ND-5000 octobus path through DOMAIN HANDLING - from monitor entry, through description-file access, to placing and executing a real domain.

**Sources**: command semantics = ND-60.136.04A (section numbers cited); expected console strings = the live J04 session log of 2026-07-19; interface-activity expectations = repo carve results (marked; see `ND500-STATUS-AND-INDEX.md`). Anything not yet byte-verified is marked TO-CARVE.

**Prerequisite state**: SINTRAN booted, logged in as SYSTEM. For phases D+ a runnable domain must exist; the NLL distribution floppy `210319H02-XX-01D` (image `ND-disk-00042.img`) entered as a directory provides one with no prior install (`(210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02`).

---

## Phase A - Monitor entry (first interface contact)

| # | Command | Expected console checkpoint | Expected interface activity |
|---|---------|------------------------------|------------------------------|
| A1 | `@ND-500` | Banner `ND-500/5000 MONITOR  Version J04 88. 6.16 / 88. 8.17` then `N500:` (exact string from live log) | Monitor background program starts; a process is allocated (section 8.7.3). On the FIRST ND-500 process the system auto-loads/starts the swapper and microcode as needed (sections 8.10.3, 8.10.10.4). 3022: control-store gate + activation kicks. Octobus/5000: ACCP presence/selftest, CS-load path, kicks 1/3/6 (verified in repo carves). |
| A2 | `EXIT` then repeat `@ND-500` | `@` prompt, then banner again | Process release + re-allocate (RESRV/RELIS path of MON 60B). Good idempotency check. |

## Phase B - Read-only status (light interface traffic, early asserts)

| # | Command | Expected checkpoint | Notes |
|---|---------|---------------------|-------|
| B1 | `VERSION` | Four version numbers: subsystem, system part, swapper, microprogram (section 8.9.7) | Microprogram version implies a read through the interface. TO-CARVE: exact mechanism. |
| B2 | `WHO-IS-ON` | List including own terminal (section 8.9.5) | ND-100-side tables only. |
| B3 | `LIST-ACTIVE-PROCESSES` | Own process (+ swapper) listed (section 8.9.6) | |
| B4 | `PROCESS-STATUS` | Per-process idle/active + CPU time (section 8.10.8.4, SYSTEM only) | |
| B5 | `LIST-STANDARD-DOMAINS` | Empty on a virgin system (section 8.2.3) | Negative control. |

## Phase C - Domain lookup WITHOUT execution (description-file path, no ND-500 run)

| # | Command | Expected checkpoint | Notes |
|---|---------|---------------------|-------|
| C1 | `LIST-DOMAIN` | On a virgin user: `DESCRIPTION FILE ERROR: DESCRIPTION-FILE` / `NO SUCH FILE NAME` (exact strings from live log) | Deterministic negative control - pure ND-100 file system, NO bus traffic expected. |
| C2 | `ND-500-LINKAGE-LOADER` (as a command at `N500:`) | `TOO LONG PARAMETER` (live log; >16-char domain-name limit) | Parser-level check, no bus traffic. |
| C3 | `LIST-DOMAIN (210319H02-XX-01D:FLOPPY-USER)` | Domain `LINKAGE-LOAD-H02` listed with start address (section 7.1.2 allows user prefix in the Monitor) | Reads the FLOPPY's description file. Still no ND-500 execution. |

## Phase D - Place and run a domain (the full activation path - the core test)

| # | Command | Expected checkpoint | Expected interface activity |
|---|---------|---------------------|------------------------------|
| D1 | `PLACE-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02` | Silent success (prompt back) | Segment mapping set up: logical->physical segments, PC := start address, trap registers initialized (section 8.1.1). Swapper messages queued. TO-CARVE: exact MON 60B subfunction order + 3022 register / 5MPM mailbox writes. |
| D2 | `LIST-ACTIVE-SEGMENTS OWN` | Segments of own process with logical->physical mapping (section 8.10.9.2) | Verifies D1's mapping from the ND-100 side. |
| D3 | `LIST-PROCESS-TABLE-ENTRY OWN` | Process description incl. program/data capabilities (section 8.10.9.4) | Capability check (direct segments, section 2.2). |
| D4 | `RUN` | NLL starts: NLL banner/prompt `NLL:` | THE activation: 3022 = activation kick to the 500 (TO-CARVE: real TAG sequence - NDBusND500IF.cs TAG protocol is FABRICATED); ND-5000 = X5ACT work flag (MAILINK+5: -1 idle -> 0 work) + octobus kick, level-12 completion back. Then a stream of MON calls from ND-500 (terminal output!) each exercising the twin-process path both ways. |
| D5 | `EXIT` (at `NLL:`) | Back to `N500:` | Normal domain termination (MON 0 path), process stays allocated. |
| D6 | `LINKAGE-LOAD-H02` typed alone (after a local copy exists) or repeat D1+D4 | `NLL:` again | Implicit RECOVER-DOMAIN lookup chain (section 8.1.2). |

## Phase E - Interrupt/resume and teardown

| # | Command | Expected checkpoint | Interface activity |
|---|---------|---------------------|--------------------|
| E1 | (Escape key during a running domain) | Back to `N500:`; files stay open (section 8.9.3) | Stop/suspend of the ND-500 process. TO-CARVE: how the stop reaches the 500 (3022 vs octobus differ). |
| E2 | `CONTINUE` | Domain resumes where stopped (section 8.1.4) | Re-activation without re-place. |
| E3 | `EXIT` | `@` | Process termination, RELIS, resources freed. |

## Phase F - Supervisor-level interface stress (optional, deeper register coverage)

Order matters: F1-F2 only with no other ND-500 users (section 8.10).

| # | Command | Expected checkpoint | Interface activity |
|---|---------|---------------------|--------------------|
| F1 | `SET-ND-500-UNAVAILABLE` | silent | None (gate flag). |
| F2 | `STOP-ND-500` | silent (section 8.10.3) | CPU stop. NEXT process start = full warm start: microcode reload + swapper place/start - repeats the whole bring-up under test control. |
| F3 | `@ND-500` + run any domain (repeat D1/D4) | NLL prompt | Complete cold-path activation, deterministic. |
| F4 | `MICRO-STOP` then `MICRO-START <addr>` | silent (sections 8.10.6.1-2) | Microprogram stop/resume with registers retained - exercises the control-store/exam-deposit register surface. |
| F5 | `LOOK-AT-HARDWARE INTERFACE` | Interface register dump (section 8.10.7.3) | DIRECT reads of the 3022 interface registers - the single best command for validating the emulator's register map. NOTE: manual requires MICRO-START afterwards. |
| F6 | `LIST-TABLE LAST-N500-MSG` | Ring buffer of the last 64 messages to ND-500 (section 8.10.9.1) | Lets the test ASSERT on the actual message traffic the monitor sent - compare against the emulator's captured mailbox writes. |
| F7 | `GET-FLAG <proc>` / `SET-FLAG <proc> <val>` (from a second terminal for a terminal-owned process) | flag value round-trip (sections 8.7.6-7) | RFLAG(100B)/SFLAG(101B) subfunctions of MON 60B - small, self-contained interface transactions, ideal as a minimal unit test. |
| F8 | `SET-ND-500-AVAILABLE` | silent | Gate reopened. |

## Phase G - Domain CREATION via NLL (write path through the description file)

Runs entirely as ND-500 execution (NLL is a 500 domain), so every NLL command also generates MON-call traffic across the interface:

```
N500: RECOVER-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02   (NLL from floppy)
NLL: COPY-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02,"LINKAGE-LOAD-H02"
NLL: LIST-DOMAIN                        (new domain visible)
NLL: EXIT                               (description file finalized, access set)
N500: LINKAGE-LOAD-H02                  (run the locally created domain - proof)
NLL: EXIT
N500: EXIT
```

This is the same sequence as the installation recovery path (see `../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md` section 4a) and doubles as its live verification.

---

## ADDENDUM: Phase H - Creating a Domain and Loading Code Into It

This is the full NLL write path: create domain -> open segment -> load NRF code -> close -> execute. All command syntax verified against ND-60.136.04A (sections cited). NLL itself runs as an ND-500 domain, so every step ALSO generates interface traffic (MON-call stream for its file I/O and terminal output).

### H-pre: where does the test :NRF file come from?

LOAD-SEGMENT consumes files in ND Relocatable Format, produced by ASSEMBLER-500 / FORTRAN-500 / PLANC-500 etc. (section 6.3). On a test system with no compilers, the options are:

- (a) Compile once on a full SINTRAN system and copy the resulting `:NRF` file onto the test disk image (deterministic fixture - recommended).
- (b) Synthesize a minimal `:NRF` byte stream in the unit test itself - DRAFTED: see `NRF-MINIMAL-SYNTHETIC-TEST-FILE.md` in this folder (14-byte file: BEG/MSA/LDI/END/EOF wrapping a single `CALL 0xF8000000,0` = MON 0 LEAVE; includes the C# builder and a 4-point validation checklist before trusting it).

### H1 - Simplest possible: no domain, load direct to memory, RUN

Needs NO domain and NO description-file write - the smallest loader/execute round trip (sections 6.2.13, 7.1.4):

```
N500: LINKAGE-LOADER                      (or LINKAGE-LOAD-H02)
NLL: DEFINE-SEGMENT-SIZE 2000B, 2000B     (only if > 64 data pages needed; optional)
NLL: LOAD-SEGMENT TEST-CODE               (no domain set -> loads straight to virtual memory)
NLL: RUN                                  (executes the loaded code; back to NLL: after)
NLL: EXIT
```

Checkpoints: LOAD-SEGMENT prints a load report (`Program: ...B P  Data: ...B D` form, section 1.1.4 example); RUN produces the test program's output. NOTE: loading with no domain/segment implicitly uses SCRATCH-DOMAIN / SCRATCH-SEGMENT-01 and DESTROYS their previous contents (section 6.2.1) - fine for tests, never for real work.

### H2 - Create a named single-segment domain

```
NLL: SET-DOMAIN "TEST-DOMAIN"             (quotes = create NEW; 1-16 chars; section 6.1.1)
NLL: LOAD-SEGMENT TEST-CODE               (implied OPEN-SEGMENT SEGMENT-Dxxx-Syy; section 6.2.1)
NLL: WRITE-DOMAIN-STATUS                  (entry not fully updated until END-DOMAIN; section 6.1.6)
NLL: EXIT                                 (END-DOMAIN + CLOSE-SEGMENT implicit: auto-link,
                                           labels to :LINK, description file updated,
                                           file access set; sections 6.1.2, 6.2.2)
N500: LIST-DOMAIN                         (TEST-DOMAIN now listed with start address)
N500: TEST-DOMAIN                         (implicit RECOVER-DOMAIN -> program runs)
N500: EXIT
```

Assert: LIST-DOMAIN shows TEST-DOMAIN; running it produces the fixture program's output. This is the description-file WRITE test in miniature.

### H3 - Multi-segment domain with a shared library segment (full write path)

Per sections 1.1.4, 6.2.1-6.2.3:

```
NLL: SET-DOMAIN "TEST-TWO-SEG"
NLL: OPEN-SEGMENT "TESTLIB", P            (quotes = new; P = shareable program segment)
NLL: LOAD-SEGMENT SUBR-CODE
NLL: CLOSE-SEGMENT                        (undefined refs auto-link here; section 6.2.2)
NLL: SET-SEGMENT-NUMBER 2                 (main segment must not collide with TESTLIB = 1)
NLL: LOAD-SEGMENT MAIN-CODE
NLL: LINK-SEGMENT TESTLIB                 (expect: "Segment no. 1 is linked")
NLL: EXIT
N500: TEST-TWO-SEG                        (runs; proves cross-segment references resolve)
```

Reusing the library from a second domain (no reload - the sharing test):

```
NLL: SET-DOMAIN "TEST-SECOND"
NLL: SET-SEGMENT-NUMBER 2
NLL: LOAD-SEGMENT MAIN2-CODE
NLL: LINK-SEGMENT TESTLIB
NLL: EXIT
```

At run time, two concurrent users of TESTLIB should map the SAME physical segment (one in-memory copy, section 2.1) - assert via `LIST-ACTIVE-SEGMENTS ALL` / `LIST-SEGMENT-TABLE-ENTRY ALL` from a SYSTEM terminal while both run.

### H4 - Adding MORE code to an existing domain (append/patch cycle)

- `APPEND-SEGMENT <segment name>` instead of OPEN-SEGMENT: adds code to an already loaded segment WITHOUT erasing it (OPEN-SEGMENT on an existing segment ERASES all old information - section 6.2.1; this asymmetry is the classic data-loss trap).
- `RESET-SEGMENT <segment name>`: clears code/labels but keeps the segment files (section 6.2.8 area).
- `DELETE-SEGMENT <segment name>`: removes segment + files; illegal while a domain is set - END-DOMAIN first (section 6.2.9).

```
NLL: SET-DOMAIN TEST-DOMAIN               (no quotes - existing domain)
NLL: APPEND-SEGMENT SEGMENT-D001-S01      (or the explicit name; keeps existing code)
NLL: LOAD-SEGMENT EXTRA-CODE
NLL: EXIT
```

### H5 - Inspection/verification commands for the test asserts

| Command | Shows | Section |
|---------|-------|---------|
| `LIST-DOMAIN` | domain exists + start address | 6.1.5 |
| `LIST-SEGMENT ,,` | all segments of all own domains + info | 6.2.11 |
| `WRITE-DOMAIN-STATUS <dom>` | everything about the domain | 6.1.6 |
| `WRITE-SEGMENT-STATUS <seg>` | everything about a segment (only complete after CLOSE-SEGMENT) | 6.2.12 |
| SINTRAN `@LIST-FILES` | the :PSEG/:DSEG/:LINK files physically exist and grew | - |

### H6 - Cleanup (teardown between test runs)

```
NLL: END-DOMAIN                           (must have NO domain set for the deletes)
NLL: DELETE-DOMAIN TEST-SECOND            (segments deleted, files retained; 6.1.4)
NLL: DELETE-DOMAIN TEST-TWO-SEG
NLL: DELETE-DOMAIN TEST-DOMAIN
NLL: EXIT
```

Note `DELETE-DOMAIN` retains the :PSEG/:DSEG/:LINK files (section 6.1.4) - delete them with SINTRAN `@DELETE-FILE` if the test needs a byte-identical starting disk. `RELEASE-DOMAIN` (section 6.1.9) exists for crash recovery when a domain is stuck open - use with care, contents unpredictable afterwards.

---

## Test-harness assertion summary

Minimum assert set for a "domain handling works" green:

1. A1 banner string + `N500:` prompt.
2. C1 exact error pair on virgin user (negative control - proves description-file path fails CLEANLY, not silently).
3. D4 `NLL:` prompt appears (proves activation, execution, and MON-call terminal output round-trip).
4. D2 shows a non-empty segment mapping for OWN.
5. G: after COPY-DOMAIN + EXIT, `LIST-DOMAIN` on the local user succeeds (proves the description-file WRITE path) and the local domain runs.
6. F6 ring-buffer contents match the emulator's captured interface messages (strongest cross-check; format TO-CARVE).
7. H2: SET-DOMAIN + LOAD-SEGMENT + EXIT creates a runnable domain from scratch (description-file write + NRF load + auto-link), and H1 (direct-memory LOAD-SEGMENT + RUN) passes even with no domain - the minimal loader/execute round trip.

3022 vs octobus: the command surface and console checkpoints are IDENTICAL; only the expected low-level traffic differs (3022 register kicks + level 12 vs ACCP/X5ACT/X5FIF + kicks 1/3/6). Keep one command script, two interface-expectation tables.
