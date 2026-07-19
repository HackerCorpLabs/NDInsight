# TASK-01 — The 16-bit password fold + login check

Do the [shared setup](README.md#shared-setup-every-task-does-this-first) first.

## Load
| Program | File | Base | Symbols |
|---|---|---|---|
| S3FS | `segments\006-S3FS.bin` | `0x2C00` (oct 26000) | `re\006-S3FS.ghidra-symbols.txt` |
| S3CP | `segments\003-S3CP.bin` | `0x3000` (oct 30000) | `re\003-S3CP.ghidra-symbols.txt` |

Load them as **two separate programs** (they overlap in address space).

**Verify:** in S3FS, `0x568F` (`RUSPW`) = `STZ ,B 0`; `0x46FE` (`DPASS`) = `SWAP SD DX`;
`0x4713` = `MON 0347`. In S3CP, `0x6196` = label `LOGIN`.

## Goal
Find and document the routine that (a) reads the typed password string and folds
it into the **16-bit password word**, and (b) compares that word against the
stored one during login.

## Verified so far (build on, don't redo)
- The stored password is a **single 16-bit word** (`norskdata-ndfs`
  `ndfs-c\include\ndfs\user_entry.h`: password at user-entry offset 18, 16-bit
  big-endian) — a fold, not a crypto hash.
- `RUSPW` (`0x568F`, S3FS) is a **dispatcher** (`JPL I` sub-handler table over a
  shared error path), not the fold.
- `DPASS` (`0x46FE`, S3FS) is a character loop that calls `MON 0347` (NUCL,
  directory access).
- `OPWCH` ("Old PassWord CHange", oct 177650 = `0xFFA8`) is **outside** the carved
  S3FS code (S3FS spans `0x2C00`–`0x9600`) — the fold is in **S3CP** (near `LOGIN`)
  or a runtime overlay.

## Steps
1. In S3CP, start at `LOGIN` (`0x6196`); follow the call graph toward the password
   prompt/verify. Resolve `JPL I` indirects by reading the literal at the target.
2. Identify the fold loop (loop over password characters producing one 16-bit
   accumulator — rotate/shift + add/xor).
3. Locate the compare against the stored 16-bit word.

## Validate
On WSL: `~/repos/norskdata-ndfs/ndfs-c/build/ndtool -u ~/repos/nd100x/SMD0.IMG`
lists users; the stored 16-bit password is readable from the user table — run the
fold you reverse-engineered on a known password and confirm it produces the stored
value.

## Deliverable
Append the algorithm (pseudocode + the ND-100 routine addresses in hex+octal,
VERIFIED/UNCERTAIN) to `versions\L-VSX-500\re\README-password-login.md` under a
new "Fold algorithm (VERIFIED in Ghidra)" section.
