# Ghidra handoff prompt — reverse-engineer SINTRAN III system segments

Copy everything below the line into a Claude running on **Windows** (where the
Ghidra MCP tools are available). All paths are Windows (`E:` = the WSL `/mnt/e`
mount). All addresses are **hex** — Ghidra works in hex, while the SINTRAN
symbol tables and manuals are octal, so both are given where it matters.

---

You have Ghidra with the **ND-100 processor module** (big-endian, 16-bit,
word-addressed) and the Ghidra MCP tools. Use the **nd100-ghidra** skill for
ND-100 RE conventions (PLANC/NPL calling conventions, data-before-code,
self-modifying code, TPE-MON patterns).

## What these files are

Carved SINTRAN III (VSX/500 **L07**) operating-system segments, extracted from
the system disk. Each `.bin` is **big-endian, native ND-100 word order** — load
it into Ghidra **as-is, no byte-swapping** (Ghidra's big-endian processor
consumes native order directly). Every segment loads at its own base address in
a **word-addressed** space.

Carved segments + metadata:
`tools/sintran-segment-carver/versions/L-VSX-500/segments/`
- `manifest.json` — per segment: `name`, `load_address_oct`, `segle` (pages),
  `madr`, `symbol_file`, `confidence`, `file`.

One-click Ghidra symbol files (format `NAME 0xADDR`, hex, ready for Ghidra's
built-in `ImportSymbolsScript.py`):
`tools/sintran-segment-carver/versions/L-VSX-500/re/`
- `006-S3FS.ghidra-symbols.txt`  (467 labels, file-system segment)
- `003-S3CP.ghidra-symbols.txt`  (3398 labels, command/login segment)

Symbol tables (octal `NAME=addr`, source of the above):
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/`
(`FILSYS-SYMBOLS`, `SYMBOL-1-LIST`, `SYMBOL-2-LIST`, `N500-SYMBOLS`, `RTLO-SYMBOLS`).

Prior findings to build on (do not re-derive):
`tools/sintran-segment-carver/versions/L-VSX-500/re/README-password-login.md`
and the full annotated disassembly `006-S3FS.annotated.dis`.

## Step 1 — load and verify S3FS (the file-system segment)

1. Import `segments\006-S3FS.bin` as **Raw Binary**, language = **ND-100
   big-endian 16-bit**, **Base Address `0x2C00`** (= octal 26000, word address).
   Do **not** byte-swap.
2. Disassemble from `0x2C00`.
3. **Verify the load** — confirm these (hex addr → instruction):

   | Address | Expect | Note |
   |---------|--------|------|
   | `0x2C00` | `JMP I 0x35` | segment entry (word `0xAA35`) |
   | `0x46FE` | `SWAP SD DX` | label `DPASS` |
   | `0x4713` | `MON 0347` | directory/nucleus call (octal MON 347) |
   | `0x568F` | `STZ ,B 0`   | label `RUSPW` |

   If `0x568F` disassembles as `STZ ,B 0`, the base, endianness, and
   word-addressing are all correct. If code appears at doubled addresses
   (~`0xAD1E`), the module is byte-addressed — re-import word-addressed.
4. Apply labels: **Window → Script Manager → `ImportSymbolsScript`** → select
   `re\006-S3FS.ghidra-symbols.txt`. `RUSPW` should land on `0x568F`, `DPASS` on
   `0x46FE` — if they sit exactly on those instructions, the load is confirmed.

## Step 2 — load S3CP (command / login segment) as a SEPARATE program

Segments overlap in virtual address space, so keep one program per segment.
- Import `segments\003-S3CP.bin`, Raw Binary, ND-100 big-endian 16-bit,
  **Base `0x3000`** (octal 30000). Disassemble from `0x3000`.
- Apply `re\003-S3CP.ghidra-symbols.txt` via `ImportSymbolsScript`.
- Landmark: label `LOGIN` at `0x6196` (octal 060616).

## Step 3 — the reverse-engineering task: the 16-bit password fold

VERIFIED so far:
- Each user's stored password is a **single 16-bit word** (confirmed by the
  `norskdata-ndfs` library, `ndfs-c\include\ndfs\user_entry.h`: password at
  user-entry offset 18, 16-bit big-endian). SINTRAN "hashing" folds the typed
  string into one 16-bit value — not a cryptographic hash.
- `RUSPW` (`0x568F`, S3FS) is a **dispatcher** (a table of `JPL I` sub-handlers
  over a shared error path), not the fold.
- `DPASS` (`0x46FE`, S3FS) is a character loop that calls `MON 0347` (NUCL) —
  directory access.
- `OPWCH` ("Old PassWord CHange", octal 177650 = `0xFFA8`) is **outside** the
  carved S3FS content (S3FS spans `0x2C00`–`0x9600`). So the fold lives in the
  **command segment S3CP** (where `LOGIN` is) or a runtime overlay.

GOAL: find the routine that (a) reads the typed password string and folds it to
the 16-bit word, and (b) compares it against the stored word during login.
1. In **S3CP**, start at `LOGIN` (`0x6196`) and follow the call graph toward the
   password prompt/verify. ND-100 calls are `JPL` (direct) and `JPL I` (indirect
   through a literal — read the word at the target to get the routine address).
2. Identify the character-folding loop (look for a loop over the password
   characters doing rotate/shift + add or xor, producing one 16-bit accumulator).
3. Confirm by cross-checking a real stored value: on WSL,
   `ndtool -u <image>` lists users; the stored 16-bit password is readable from
   the user table to validate the fold's output.
4. Document the algorithm in
   `tools/sintran-segment-carver/versions/L-VSX-500/re/README-password-login.md`
   (append a "Fold algorithm (VERIFIED in Ghidra)" section). Keep the octal
   symbol addresses alongside hex so it ties back to the symbol tables.

## Reading tips (ND-100)
- Word-addressed, big-endian, one instruction per word. Data and jump tables
  interleave with code — a run of `JPL I` entries is usually a dispatch table.
- `MON nnn` is octal 161000–161377; the low 8 bits are the monitor-call number.
- Radix: symbol tables and SINTRAN docs are **octal**; Ghidra is **hex**. E.g.
  `RUSPW` = octal 053217 = `0x568F`. Convert when moving between them.

## Optional: MON-call dispatch segments
The `S3MPIT`/`S3RPIT` segments and `S3IMAGE` have `madr=0` (no distinct SEGFIL0
disk image), so they do not carve. The resident level-14 dispatch code is
captured under **`116-S3SERWD.bin`** instead (verified by a `GOTAB` content
signature). Load it Raw Binary, ND-100 big-endian 16-bit, **Base `0x600`**
(octal 3000), symbol tables `SYMBOL-1-LIST` + `SYMBOL-2-LIST`. Landmarks:
`ENT14` = octal 072167, `GOTAB` = octal 071233 (256-word jump table), the
dispatch `JMP ,X` at octal 072260 (`0x74B0`), mask literal `000377` at 072266.
Full walkthrough: `SINTRAN\OS\23-MON-CALL-DISPATCH-DEVELOPER-GUIDE.md`.
