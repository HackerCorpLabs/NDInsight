# State of play: DESC format resolved, NRF LDN fix pending commit

**Date:** 2026-08-11
**Audience:** whoever picks this thread up next - a WSL session in `nd500x`/`pcc-nd500`, a
Windows session driving Ghidra, or a fresh session with none of this context.
**Supersedes as the entry point:** `HANDOFF-NRF-LDN-PARSER-BUG-2026-08-11.md` (still the
authoritative record of the LDN bug itself - read it for that; read this for current state).

Paths to files inside this repository are repository-relative. Files in other repositories are
named by repository + relative path, never by drive letter.

---

## 1. Read this first: commit state

**Updated 2026-08-17.** The LDN fix and this documentation set are now committed; only the
nd500x settings question is still open.

| Repository | Uncommitted | Note |
|---|---|---|
| `pcc-nd500` | none - the LDN fix is commit `c82cbfc` (2026-08-17) | untracked `SCRATCH/` and an `nrf_test` binary remain in the working tree and must NOT be committed |
| NDInsight (this repo) | none of the File-Formats set - committed 2026-08-17 on branch `5000x` | **Other sessions are editing XMSG C# sources, Hardware 3D models and `tools/ghidra-planc/` in this repo right now, and their work is still uncommitted. Never `git add -A` here. Add exact paths and check `git status --short` before and after.** |
| `nd500x` | `.claude/settings.json` | the `Bash(kill:*)` deny removal; Ronny's call to commit or revert |

The nd500x DAP work IS committed there as `cf6d83c`, with `docs/HANDOFF-DAP-MONITOR-SHELL-2026-08-11.md`.

## 2. What is settled, and how strongly

### DESCRIPTION-FILE:DESC segment entry - RESOLVED

Ten field offsets confirmed from the ND-500 Monitor's own code (`MON-DEBUG:PROG` J04), which
reads this file and prints each field beside its own label. Full evidence per field, with the
loading instruction address, is in `SINTRAN/ND500/nd-500-mon/CARVE-ANSWER-DESC-FIELD-OFFSETS-2026-08-11.md`;
the layout is written up in `DESCRIPTION-FILE-FORMAT.md` and `desc-format.json`.

**The size rule, verified 4/4 on both axes across two independently produced DESC files:**

```
PLB + PSIZE + 1 = .pseg file size
DLB + DSIZE + 1 = .dseg file size
```

| Segment | PLB | PSIZE | `.pseg` | DLB | DSIZE | `.dseg` |
|---|---|---|---|---|---|---|
| SCRATCH-SEG-01 (both floppies) | 0 | 4 | 5 | 0 | 1028 | 1029 |
| LINKAGE-LOAD-H02 | 0 | 123988 | 123989 | 75834 | 2109142 | 2184977 |
| LED-B03 | 0 | 223694 | 223695 | 0 | 394524 | 394525 |

This is why every historic byte-value search for the literal file sizes failed: the file stores
the last byte index, not a count.

**A correction that matters more than it looks:** an earlier version of section 5 listed
LINKAGE-LOAD-H02's DSIZE as 2,109,654. That was an arithmetic slip reading `00 20 2e d6`; the
value is **2,109,142**. The wrong number created a fictitious "open anomaly" (DSIZE not matching
the `.dseg`) that survived into section 6 and into a task list. With the correct value the entry
obeys the same rule as every other. **If a document you are reading still calls this an open
anomaly, that document is stale.** The offset label in
`HANDOFF-NRF-LDN-PARSER-BUG-2026-08-11.md` was also wrong and was corrected on 2026-08-17:
these bytes are at `0x4124`, not `0x4120`. Re-read from the real file that day, segment entry
at `0x40C0`: `+88` PLB `00 00 00 00`, `+92` PSIZE `00 01 e4 54`, `+96` DLB `00 01 28 3a`,
`+100` DSIZE `00 20 2e d6`, `+104` DEBUGINFO `00 02 66 8b`.

### File geometry - RESOLVED

2048-byte pages: 256-byte header + 32 domain entries of 56 bytes. Domain entry *index* sits at
`56*index + 256*(index div 32 + 1)`, so entry 0 is at byte 256. This is the monitor's own
arithmetic and it lands exactly on the domain names in both real files. It also explains the old
"fields sum to 54 but entries are 56 apart" puzzle - the entry is 56 because 32 of them plus the
header fill a page.

Segment entries are a **singly linked list**: word 0 of a domain entry is the file byte position
of its first segment entry, word 0 of a segment entry points to the next, 0 ends the chain.
Verified in both files.

### NRF LDN - FIXED, NOT YET INDEPENDENTLY RE-VERIFIED

Control number 27's numeric field is a byte **count**, with that many raw payload bytes following
the header; every other control group's numeric field is its whole payload. Both the C parser
(`pcc-nd500`, `src/lib/nrf/nrf_utils.c`) and the viewer's JS port now skip the payload. The count
must be recomputed from the raw bytes - the stored `numeric_value` is sign-extended, so an LDN
with `NL=1` and the top bit set would come out negative. The previous session reports all four
library files parsing to clean EOF. **The next session should re-run that before committing**
(todo #1), not because the work looks doubtful but because "verified by another agent" is not
verification.

## 3. Dead ends - do not spend time here again

- **CONVERT-DOMAIN is not a witness for the DESC size fields.** Patching PSIZE from 123988 to
  16384 in a real DESC and re-running `CONVERT-DOM-A03` under nd500x produced a byte-identical
  2,316,049-byte `.DOM`. It queries the filesystem with MON 62B GetBytesInFile instead of reading
  the entry.
- **The DESC read does not go through RFILE.** It is MON 74 SETBT (seek) plus a MON 1 INBT byte
  loop (`013527B` -> `013406B`). An earlier brief guessed the `176740B` RFILE helper; that was
  wrong.
- **Static byte-value scanning of the DESC file** was tried exhaustively by an earlier session and
  could not work, for the size-1 reason above.
- **NLL's `WRITE-DOMAIN-STATUS` / `LIST-DOMAIN` printing nothing** was chased at length without
  resolution. The monitor carve made it unnecessary.

## 4. Genuinely open

1. **Domain-entry field offsets past DNAME** still rest on the manual's field order. The monitor
   prints domain-entry doubles at words `0, 17B, 21B, 23B, 25B, 30B, 32B` with labels `$Domain :`,
   `  Start address:`, `$Owner:`, `  Childindex:` (bank-2 strings `0x80C6`-`0x8140`). Pairing them
   the same way the segment entry was done would confirm or refute STADR/ENABLEINT/THA/SYSENABL/
   PBITMAP/DBITMAP. This is the last unverified part of the record.
2. **Segment entry bytes 74-84**: the manual says `COMSEGSIZE` / `N100SEGNO` arrays; the monitor
   prints two byte strings there using a count at word `37B`. Both cannot be right. Recorded as
   unadjudicated in both the `.md` and the `.json` - keep it that way until settled.
3. **Write-side proof of the size rule.** The monitor only displays; it never adjusts. The rule is
   proven from files, and the monitor's reader uses the same inclusive-last-index convention twice
   (`277B`=191 for the 192-byte record, `67B`=55 for the 56-byte entry), but the writer is NLL.

## 5. Two nd500x defects found while doing this

Neither was chased; both are real and reproducible.

- **A converted `.DOM` was truncated to 0 bytes.** `LINKAGE-LOAD-H02.DOM` was a valid 2,316,049-byte
  file and was later found at 0 bytes. That is the signature of the MON 0B LEAVE segment-mapped-file
  writeback bug recorded as fixed on 2026-07-25 - check whether it regressed. It destroys a
  converted domain silently.
- **Quote-create over an existing file returns the wrong SINTRAN error.** `CONVERT-DOMAIN`'s
  quote-create of an existing (0-byte) destination failed with `056B "No such file name"`; the
  correct code is `076B "File already exists"` (`ndmonlib` already has `MON_ERR_FILE_ALREADY_EXISTS`
  in `mon_221B_CreateFile.c`). The wrong code sent this session down a false trail; moving the stub
  aside made the conversion succeed immediately.

## 6. Traps that cost real time

- The shipped `nd-500-mon-j04.prog.asm` **disassembles pointer words as instructions** (e.g. at
  `016277B` it prints `MIN ,B -44`, which is the pointer to the `DESCRIPTION-FILE` string). Literal
  pools at `014714B`-`014735B`, `013544B`-`013551B`, `016264B`-`016277B`, `016462B`-`016506B`.
- P-relative effective address = **address of the instruction** + displacement, not
  next-instruction-relative.
- `JPL` is opcode `0o134`, not `0o130`. Wrong value silently finds no calls and reads like a finding.
- `LDF`/`STF` in this program move 3-word PLANC descriptors (pointer + length), not floats.
- Bank 1 (code) and bank 2 (data) **both base at word 0**; a bank-1 pointer word holding `040734B`
  refers to the bank-2 string at that address. Import them as two Ghidra programs.
- **WSL cannot drive the Windows Ghidra.** WSL2 cannot reach Windows loopback, and nothing was
  listening on the LAN address either (all of 8000-8200 closed). There is no `ghidra` MCP entry in
  any config on the WSL side. Driving Ghidra needs a Windows-side session, or the GhidraMCP plugin
  bound to `0.0.0.0` with a firewall rule for the WSL subnet.

## 7. Where things are

| What | Where |
|---|---|
| DESC spec + machine-readable schema | `SINTRAN/File-Formats/DESCRIPTION-FILE-FORMAT.md`, `desc-format.json` |
| Monitor carve evidence (per-field addresses) | `SINTRAN/ND500/nd-500-mon/CARVE-ANSWER-DESC-FIELD-OFFSETS-2026-08-11.md` |
| The brief that drove it (import parameters, traps) | `SINTRAN/ND500/nd-500-mon/CARVE-BRIEF-DESC-FIELD-OFFSETS-2026-08-11.md` |
| LDN bug record | `SINTRAN/File-Formats/HANDOFF-NRF-LDN-PARSER-BUG-2026-08-11.md` |
| Viewer | `SINTRAN/File-Formats/viewer/`, launch with `run.bat`, **port 8888** |
| Monitor binary + banks + disassembly | `SINTRAN/ND500/nd-500-mon/` |
| nd500x DAP-under-`--monitor` fix | `nd500x` repo, commit `cf6d83c`, `docs/HANDOFF-DAP-MONITOR-SHELL-2026-08-11.md` |
| Real DESC test files | NLL H02 installer floppy `210319H02-XX-01D`; LED floppy `211160B03-XX-01D` |

## 8. Suggested order

1. Re-verify the LDN fix, then commit `pcc-nd500` (exact path only).
2. Commit the NDInsight File-Formats set by exact path.
3. Fix the `0x4120`/`0x4124` citation; make sure the size rule is stated as
   `PLB+PSIZE+1 = .pseg` / `DLB+DSIZE+1 = .dseg` everywhere, and that no document still calls the
   LINKAGE-LOAD-H02 entry anomalous.
4. Carve the domain-entry offsets - last gap, method already proven.
5. Adjudicate bytes 74-84.
6. The two nd500x defects, in that repo.
