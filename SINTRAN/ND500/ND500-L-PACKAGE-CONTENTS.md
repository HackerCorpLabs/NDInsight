# ND-211305 (ND-500/5000 System Package for SINTRAN III/VSX, version L) — Package Contents

> Status: VERIFIED. Answers Q1 of
> [ND500-L-RELEASE-RE-TASK-HANDOFF.md](ND500-L-RELEASE-RE-TASK-HANDOFF.md) section 4
> and closes the open task in
> [ND500-RE-SESSION-2-HANDOFF.md](ND500-RE-SESSION-2-HANDOFF.md) Item D.

## Source

User-reported directory listing from floppy image **`211305B02-XX-01D`**, obtained by
copying/listing the floppy directly (2026-07-09). This is primary evidence — a live
listing of the actual distribution floppy, not derived from the release doc.

## File listing

| # | User | File | Type | Pages (octal) | Bytes |
|---|------|------|------|------|-------|
| 000 | FLOPPY-USER | ND-500-MON-J04 | :PROG | 000174 | 253356 |
| 001 | FLOPPY-USER | SWAPPER-K01 | :DSEG | 000153 | 218117 |
| 002 | FLOPPY-USER | SWAPPER-K01 | :PSEG | 000023 | 38161 |
| 003 | FLOPPY-USER | PLACE-1BANK-C01 | :BRF | 000005 | 10127 |
| 004 | FLOPPY-USER | PLACE-2BANK-C01 | :BRF | 000006 | 10281 |
| 005 | FLOPPY-USER | PLACE-BIG-1B-C01 | :BRF | 000021 | 34745 |
| 006 | FLOPPY-USER | PLACE-BIG-2B-C01 | :BRF | 000022 | 36236 |
| 007 | FLOPPY-USER | PLACE-SML-1B-C01 | :BRF | 000004 | 7136 |
| 008 | FLOPPY-USER | PLACE-SML-2B-C01 | :BRF | 000004 | 7928 |

## Observations

- All files live under user **FLOPPY-USER** on the floppy (not SYSTEM) — matches the
  general BPUN/floppy-file convention noted in
  `../../Installation/OS/research/HDD-IMAGE-FINDINGS.md` section 6
  ("Floppy/util `:BPUN` -> (FLOPPY-USER)"), though these are `:PROG`/`:DSEG`/`:PSEG`/`:BRF`,
  not `:BPUN`.
- Only **one** Monitor file (`ND-500-MON-J04:PROG`) and **one** Swapper pair
  (`SWAPPER-K01:PSEG`/`:DSEG`) — no per-CPU-model variants, confirming the release doc's
  "version J04 or later" / "version K" statement (lines 2721, 3547-3551) maps to exactly
  these files (J04, K01).
- **Six** Place Library `:BRF` variants, not one:
  `PLACE-1BANK-C01`, `PLACE-2BANK-C01`, `PLACE-BIG-1B-C01`, `PLACE-BIG-2B-C01`,
  `PLACE-SML-1B-C01`, `PLACE-SML-2B-C01`. `PLACE-BIG-2B-C01.BRF` is the variant already
  opened in Ghidra by [ND500-PLACE-LIBRARY-C9-FINDINGS.md](ND500-PLACE-LIBRARY-C9-FINDINGS.md)
  (confirmed there: ND-100 object code, language ND-100:BE:16, not ND-500 domain code).
  UNVERIFIED/ASSUMPTION: the `1BANK`/`2BANK` split likely mirrors the bank-count
  distinction seen in other ND-100 language products (e.g. the Pascal J worked example in
  `../../Installation/Software/INSTALL-METHODOLOGY.md` section 5, where the compiler is a
  two-bank program needing `PASCAL-2LIB`); `BIG`/`SML` likely correspond to a
  full-diagnostics build vs a minimal build. Neither pairing is confirmed by any source
  read — do not treat as fact.
- **No explicit copy/install command exists in the release doc for the Place Library
  files.** The release doc (`../Release-Documentation/ND-860230-6-EN Sintran III -
  Release Information - L-Version.md`, lines 733-741) only gives `@COPY-FILE` commands
  for the Monitor and Swapper. UNVERIFIED how/whether the Place Library `:BRF` files are
  separately copied, or whether they are already linked into `ND-500-MON-J04:PROG` at
  build time (in which case no separate install step is needed for them at all — this
  would explain their absence from the release doc's copy-command list). Not confirmed
  by disassembly.
- No MIC-5xxx microcode file was present in this floppy image — consistent with the
  release doc treating ND-5000 microcode as a **separate** diskette (section 1.2 of the
  task handoff), not part of ND-211305 itself.

## Live-system confirmation (2026-07-09)

Copying `ND-500-MON-J04:PROG` from this floppy and running `@ND-500` on a live L system
**worked** — the Monitor loaded and printed its banner:

```
ND-500/5000 MONITOR  Version J04 88. 6.16 /     6. 6
ND-500(0) error:      No ND-500(0) CPU found
```

This confirms:
1. `ND-500-MON-J04:PROG` is a self-contained, directly-runnable SINTRAN background
   program — invoked with `@ND-500` (or `@ND-500-MONITOR`), per
   `../../Reference-Manuals/ND-60.136.04A ND-500 Loader Monitor.md` lines 729-822.
   **No RT-LOADER/HENT-MODE load step is needed for the Monitor `:PROG` itself** — see
   the correction in `../../Installation/OS/versions/SINTRAN-L.md` section 4.
2. The "No ND-500(0) CPU found" error is a separate, later-stage failure: SINTRAN's
   boot-time hardware presence test (`CH5CPUPRESENT`, `../ND500-BUS-INTERFACE-REFERENCE.md`
   section 8.1) never flagged a CPU present (`CPUAVAILABLE`/`5ALIVE`), so the Monitor's
   own runtime check refuses to proceed. This is a hardware/interface-detection issue
   (3022/5015 card or Octobus/Samson link not answering, or not configured in an
   emulator), not a software-install issue.
