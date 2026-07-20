# CARVE PROMPT: ND-500 Monitor - Domain Creation, Placement and Loading Path

**Purpose**: prompt for the carving/RE assistant. Goal is a byte-verified map of what the ND-500 Monitor (background program + SINTRAN-resident part) actually DOES for domain creation/placement/loading, so the RetroCore emulator can be validated against it. Written 2026-07-19 after a live NLL installation on the emulator failed silently (see `../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md`, gotcha G12).

---

## The prompt

You are carving SINTRAN III and the ND-500 Monitor. Before anything else, read the two status-of-record files (`CARVING-HANDOFF.md` and `ND500-STATUS-AND-INDEX.md`) and obey their discipline: byte-verified facts only, cite segment + octal address for every claim, mark anything inferred as ASSUMPTION, and update the status files with what you find.

### Context and symptoms (observed on the emulator, SINTRAN ND-500/5000 MONITOR Version J04 88.6.16/88.8.17)

1. `N500: LINKAGE-LOADER` fails with `DESCRIPTION FILE ERROR: DESCRIPTION-FILE / NO SUCH FILE NAME` (expected on a virgin user - but we need to know exactly which file open, on which user, produces this).
2. Names longer than 16 chars at the `N500:` prompt give `TOO LONG PARAMETER` (domain-name length check - locate it to anchor the command parser).
3. The NLL installer printed `Copying the domain: LINKAGE-LOAD-H02 to user DOMAIN-USER` and `INSTALLATION FINISHED`, yet NO files appeared on DOMAIN-USER. Either the emulator mis-executes part of the path, or the installer's cross-user copy uses machinery we have not carved. We need the real mechanics.

### What to carve, in priority order

**1. The Monitor's command processor and command table.**
Locate the ND-500 Monitor background program's command dispatch (the `N500:` prompt loop). Produce: the command table location and format, and the handler entry points for at least PLACE-DOMAIN, RECOVER-DOMAIN, LIST-DOMAIN, LIST-STANDARD-DOMAINS, DEFINE-STANDARD-DOMAIN. Identify where the implicit-RECOVER-DOMAIN fallback (typed name -> standard domains -> own domains -> SYSTEM domains -> macros) is implemented and where the 16-char `TOO LONG PARAMETER` check fires.

**2. DESCRIPTION-FILE:DESC handling.**
Manual ND-60.136.04A chapter 11 ("DESCRIPTION FILE LAYOUT") documents the layout - use it as the cross-check, not as the source of truth. Carve: which routine opens `DESCRIPTION-FILE:DESC` (whose user, what access mode, which MON calls), the domain-entry and segment-entry record structures as the CODE reads them (field offsets, name lengths, segment-number table), and what "create a new domain entry" writes. This yields the exact failure point behind symptom 1 and tells us what a minimally valid description file must contain for the emulator.

**3. The PLACE-DOMAIN -> RUN path down to the hardware.**
Trace what happens between `RECOVER-DOMAIN <name>` and the first ND-500 instruction executing, on the ND-100 side:
- Which MON 60B (N500M) subfunctions are invoked, in what order, with what parameter blocks (RESRV and RELIS are known names; identify the rest by number). Remember the PIT overlay truth: MON 60B lives at 26000B-37777B in overlay 050-S3I5PIT (PIT 5) - carve there, and never trust GOTAB; the real dispatch is MCTAB@005620B.
- How the segment mapping is set up (logical segment -> physical segment, the :PSEG/:DSEG file mapping, swap-file scratch allocation), and which messages go to the swapper (the LAST-N500-MSG ring buffer format = the messages to ND-500).
- The final hardware kick: the exact 3022 interface register sequence (IOX addresses and order) and/or 5MPM mailbox writes and the level-12 handshake. WARNING: the TAG-code protocol currently in the emulator's NDBusND500IF.cs is FABRICATED - carve the real sequence; carved bytes outrank the PDF where they disagree.

**4. Standard-domain table.**
Locate the table DEFINE-STANDARD-DOMAIN writes (it survives warm start but not cold start, so it lives on a resident/monitor segment): address, entry format, capacity. This lets the emulator persist/lose it at the right times.

**5. Cross-user domain copy (the installer mystery).**
The manual gives NLL no way to create a domain under ANOTHER user (SET-DOMAIN/COPY-DOMAIN always target the current user). The NLL installer (`IN-NLL-XX-H02:PROG`, an ND-100 program) claims to copy a domain to DOMAIN-USER anyway. Disassemble the installer binary if available, or carve whatever service it invokes, and determine: does it do plain @COPY-FILE of :PSEG/:DSEG/:LINK plus a description-file edit, does it run NLL under the hood, or something else? This explains the silent failure and tells us what the emulator must support.

### Deliverables

1. A golden-path document (mon-analysis style): `N500: RECOVER-DOMAIN` end-to-end with every routine, table, MON call and IOX touch, byte-cited.
2. Test vectors for the emulator: the MON 60B parameter blocks and the IOX/mailbox sequences as concrete byte/register expectations (Ghidra addresses in HEX).
3. The description-file record layout as a struct (offsets in octal words), verified against chapter 11 and flagged where the code and manual disagree.
4. A divergence checklist: each point where RetroCore (NDBusND500IF.cs / ND500ControlII / Mpm5Memory) currently differs from the carved truth.
5. Updates to `CARVING-HANDOFF.md` and `ND500-STATUS-AND-INDEX.md`.

### Hard rules

- Byte-verified or marked ASSUMPTION - no middle ground. If a dispatch target cannot be resolved, say so explicitly rather than proposing a plausible one.
- Pick overlays by sibling coherence; validate any table against known slots before trusting it (see sintran-carving traps).
- Do not "confirm" the emulator's existing TAG protocol - it is known-fabricated; only the carved sequence counts.
- Every address in deliverables for Ghidra work in HEX; SINTRAN-side addresses in octal with B suffix.
