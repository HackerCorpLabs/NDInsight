# TASK-04 — File-system internals (S3FS)

Do the [shared setup](README.md#shared-setup-every-task-does-this-first) first.

## Load
| Program | File | Base | Symbols |
|---|---|---|---|
| S3FS | `segments\006-S3FS.bin` | `0x2C00` (oct 26000) | `re\006-S3FS.ghidra-symbols.txt` |

**Verify:** `0x568F` (`RUSPW`) = `STZ ,B 0`; `0x46FE` (`DPASS`) = `SWAP SD DX`.

## Goal
Map the SINTRAN file-system segment beyond login (TASK-01 covers password/login):
directory lookup, file open/close, page/block allocation, and the on-disk
directory/object structures the code manipulates.

## Context
- `S3FS` (segment 6) is the file-system code segment; `S3SFS` (seg 12) is its save
  copy; `S3RFAC` (seg 22, `022-S3RFAC.bin`, base 0x2C00) is remote file access.
- The NPL file-system SOURCE is NOT in the repo (only `FILSYS-SYMBOLS`), so this
  segment IS the primary source. Symbol table: `FILSYS-SYMBOLS.SYMB.TXT` (L07).
- Cross-check on-disk structures with `norskdata-ndfs` (it already models the NDFS
  directory/user/object entries): `~/repos/norskdata-ndfs/ndfs-c/include/ndfs/`.
- Repo docs: `..\..\SINTRAN\OS\15-DISK-IO-SUBSYSTEM.md`,
  `..\..\SINTRAN Structures\SINTRAN-STRUCTURES.md`.

## Steps
1. From the `FILSYS-SYMBOLS` labels, group routines: directory (`ENTER`, `RUSER`,
   `RUSPW`, object lookup), open/close, allocation (bit-file / page allocation),
   and the MON-call entry points that land here (RFILE/WFILE/OPEN etc.).
2. Recover the on-disk object/directory entry layout as the code reads/writes it;
   reconcile with the `norskdata-ndfs` structs (confirm or correct field offsets).
3. Note the `MON 0347` (NUCL) usage seen in `DPASS` and elsewhere.

## Deliverable
Write `versions\L-VSX-500\re\TASK-04-results.md`: a routine map (name → addr
hex+oct → function), the directory/object structure layout with field offsets, and
any discrepancies with `norskdata-ndfs`, VERIFIED/UNCERTAIN.
