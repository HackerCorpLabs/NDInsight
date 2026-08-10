# ND-10760A — C-Compiler for ND-100, version A

> Status: IN-PROGRESS — install procedure read verbatim off the real floppies   ·   Install source: [MODE]

| Field | Value |
|-------|-------|
| Part number | `10760A00` (as printed on the floppy volume labels; whether `00` is a revision digit or part of the version tag is not confirmed — no PD sheet to check against) |
| Base product | [`ND-10760`](../README.md) |
| Version | A |
| Release date | files on disk are dated 1984-03-26 (compiler banks, headers) and 1984-11-26 (linked `:PROG` images) — no PD sheet to confirm an official release date |
| CPU target | ND-100 |
| OS requirement | unknown — no PD sheet |

## Description
CC-100, the ND-100 C compiler. Ships as a two-disk set. `INSTALL-1:MODE`, `INSTALL-2:MODE`, and
`CSESSION:MODE` were extracted from the actual floppy images (downloaded from the ND floppy
library by MD5 hash and read with `ndtool`) and decoded — SINTRAN mode/command files on this
media are stored with the high bit set on every byte; masking it off (`byte & 0x7F`) recovers
plain text. `[MODE]`

## Prerequisites
Not stated on the floppy (no PD sheet exists for this product). Two are inferred from the install
script itself:
- A user named `C-INCLUDE` must exist to receive the header files (see procedure below) — this
  mirrors the CC-500 PD sheet's explicit `<C-Include>` user requirement, which is a strong
  cross-check that this reading of the script is correct.
- Both `INSTALL-1:MODE` and `INSTALL-2:MODE` open with `@BACKUP-SYSTEM` — see
  [ND-210337 Backup-System](../../ND-210337/README.md), which must already be installed. `[INF]`

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `10760A00-1` | `100-FCNTL:H`, `100-SETJMP:H`, `100-VARARGS:H`, `100-CTYPE:H`, `100-ERRNO:H`, `100-MATH:H`, `100-STDIO:H` (7 C-Include headers) · `CC-COM-100:PROG` (112 pages) · `CC-BRF-100:PROG` (14 pages) · `INSTALL-1:MODE` |
| `10760A00-2` | `CC-1BANK-A:BRF`, `CC-2BANK-A:BRF` (compiler code, bank 1 + bank 2) · `CC-1HEADER-A:BRF`, `CC-2HEADER-A:BRF`, `CC-1TRAILER-A:BRF`, `CC-2TRAILER-A:BRF` (NRL link header/trailer per bank) · `CC-100-A:PROG` (51 pages — the compiler program itself) · `CAT:C` (example C source) · `CSESSION:MODE` (worked compile+link example) · `INSTALL-2:MODE` |

Confirmed directly by mounting both images with `ndtool -t` — file names, sizes, and dates match
the file listing exactly.

## Installation procedure

### Disk 1 — `INSTALL-1:MODE`, decoded verbatim `[MODE]`
```
@BACKUP-SYSTEM
COPY
DIR

C-INCLUDE
DIR
10760A00-1
FLOPPY-USER
:H
L
COPY
DIR


DIR
10760A00-1
FLOPPY-USER
:PROG
L
EXIT
```

### Disk 2 — `INSTALL-2:MODE`, decoded verbatim `[MODE]`
```
@BACKUP-SYSTEM
COPY
DIR


DIR
10760A00-2
FLOPPY-USER
:BRF
L
COPY
DIR


DIR
10760A00-2
FLOPPY-USER
:PROG
L
EXIT
```

### Reading of the script `[INF — BACKUP-SYSTEM's own User Guide, ND-60.250, is not in this repo, so exact prompt wording is not cross-checked]`

Both scripts drive `@BACKUP-SYSTEM`'s interactive `COPY` dialog twice, once per file-type group.
Each `COPY` answers a fixed sequence of prompts: source type (`DIR` = the currently entered
floppy directory), destination user (blank = your current logged-in user; an explicit name if
given), then the source directory name, source user, a file-type filter, and a trailing flag
(`L`). Reading disk 1 that way:

1. **Copy the `:H` header files** into a **separate user `C-INCLUDE`** (must exist first — same
   requirement as CC-500's install sheet). Source: directory `10760A00-1`, user `FLOPPY-USER`,
   type `:H`.
2. **Copy the `:PROG` files** (`CC-COM-100`, `CC-BRF-100`) to the **default/current user** (no
   destination user given) — i.e. wherever you are logged in when you run this, typically
   `SYSTEM` per the convention used elsewhere in this repo's install docs.

Disk 2 copies both its `:BRF` files (the compiler's linkable object banks) and its `:PROG` file
(`CC-100-A`, the runnable compiler) to the **default/current user** — no `C-INCLUDE` step here,
since disk 2 carries no headers.

> **Procedure to run, in your own words:** enter the `10760A00-1` floppy directory, run
> `INSTALL-1:MODE` (creates the header set under `C-INCLUDE` and the two `:PROG` support files
> under your working user), swap to the `10760A00-2` floppy, enter that directory, run
> `INSTALL-2:MODE` (copies the compiler's `:BRF` banks and the runnable `CC-100-A:PROG`).

## Verifying the install — `CSESSION:MODE`, decoded verbatim `[MODE]`

Disk 2 also carries a worked compile-and-link example, effectively the product's own smoke test:
```
@CC-100 CAT:C
@NRL
IMAGE 100
PROG-FILE "CAT"
LOAD CC-2HEADER
LOAD CAT
LOAD CC-2BANK
LOAD CC-2TRAILER
EXIT
```
This compiles the shipped `CAT:C` example with `CC-100`, then links it via **NRL** (the same
relocating loader used throughout this repo's generic install methodology, see
[../../INSTALL-METHODOLOGY.md](../../INSTALL-METHODOLOGY.md) §3–4) against `CC-2HEADER`,
`CC-2BANK` (the **bank-2** runtime library — as opposed to bank 1, which builds the compiler
itself), and `CC-2TRAILER`, producing a `CAT:PROG`. Running this after install and getting a
working `CAT` program is the closest thing this product has to a documented verification step.

## Configuration / post-install
No start command, no boot-mode hook — this is a per-user compiler install, not a resident
subsystem. No `@DUMP-REENTRANT` step is present in either script.

## Documentation
- PD-sheet: not located
- PI-sheet: not located
- Manual(s): `ND-60.214.01` CC-100 and CC-500 C-Compiler User Manual
- NDWIKI: not checked yet

## Provenance & open items
- Source: both floppy images downloaded from the ND floppy library (`https://ndlib.hackercorp.no/images/<md5>.img`,
  hashes `f5b0746106fe7355fba5634d35b0cb4a` for `10760A00-1` and `4bf30edb5e1ed89038fc1e23e2b9e57b`
  for `10760A00-2`) and read with `ndtool` (`E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build_win\ndtool.exe`).
  `INSTALL-1:MODE`, `INSTALL-2:MODE`, `CSESSION:MODE` decoded by masking the high bit off every
  byte (`byte & 0x7F`) — this is SINTRAN's standard packed-text encoding for command files on this
  media, not a guess: the result is clean, grammatical SINTRAN command syntax with no artifacts.
- **TODO:** the exact meaning of the `DIR`/blank/`L` tokens in the `BACKUP-SYSTEM COPY` dialog is
  read from context (cross-checked against CC-500's PD sheet requiring the same `<C-Include>`
  user), not from the `BACKUP-SYSTEM` User Guide (`ND-60.250`), which is not in this repo. If that
  manual turns up, re-verify the field-by-field prompt labels.
- **TODO:** confirm the `ND-10760` article number against a real PD/PI sheet if one turns up.
- **TODO:** this install has not yet been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10760` product overview)
