# ND-10609 — COSMOS Programmers Library

> Status: INSTALLED 2026-08-18 — the product's own installer `COS-LIB-IN-B02:PROG` was run on
> D100 and copied all 24 files to `(PACK-ONE:SYSTEM)` ("Installation completed"). See
> "Installing on a machine" below for the confirmed procedure. All 24 files are also kept in
> [files/](files/) (text includes as parity-stripped `.readable.txt`); the real `XMP-B02:IMPT`
> settled every open XMP declaration question — see
> [COSMOS-XMP-LIBRARY.md](../../../Developer/Languages/Application/COSMOS-XMP-LIBRARY.md)
> section 5. Note the real include names carry the version (`XMP-B02:DEFS`, `XMP-B02:IMPT`),
> not the manual's bare `XMP:DEFS`/`XMP:IMPT`.

| Field | Value |
|-------|-------|
| ND article number | `ND-10609` |
| Product name | COSMOS Programmers Library |
| Functional category | Networking & Communications (programmer API) |
| Callable from | PLANC, FORTRAN |
| Related products | Direct programmer-level access to the same `XMSG` system this repo has extensively reverse-engineered at the wire-protocol level — see [../../../SINTRAN/XMSG/README.md](../../../SINTRAN/XMSG/README.md) |

## What is known — real floppy, decoded

Floppy `10609B02-XX-01D` (image + NDFS listing in the
[Norsk Data Software Archive](https://github.com/HackerCorpLabs/norskdata-software-archive)
product `ND-10609`, local `/home/ronny/repos/norskdata-software-archive`; imaged by Torfinn
"Tingo" Ingolfsen; wiki [ndwiki.org/wiki/ND-10609B](https://www.ndwiki.org/wiki/ND-10609B))
mounts cleanly. It carries a real installer plus five call-library modules, each shipped for both
languages the PI sheet names ("Use of XMSG from PLANC/FORTRAN", "Use of TLIB from PLANC/FORTRAN"):

| Module prefix | Files | Interpretation |
|---|---|---|
| `XMP` | `:DEFS`, `:IMPT`, `-100-1-B02:BRF`, `-100-2-B02:BRF`, `-500-B02:NRF` | **X**MSG for **P**LANC |
| `XMF` | `:DEFS`, `-100-1-B02:BRF`, `-100-2-B02:BRF`, `-500-B02:NRF` | **X**MSG for **F**ORTRAN |
| `TLP` | `:DEFS`, `:IMPT`, `-100-1-B02:BRF`, `-100-2-B02:BRF`, `-500-B02:NRF` | **T**LIB for **P**LANC |
| `TLF` | `:DEFS`, `-100-1-B02:BRF`, `-100-2-B02:BRF`, `-500-B02:NRF` | **T**LIB for **F**ORTRAN |
| `RRP` | `:DEFS`, `:IMPT`, `-100-1-B02:BRF`, `-100-2-B02:BRF`, `-500-B02:NRF` | a third API, PLANC-named only — no `RRF` (FORTRAN) counterpart shipped; not identified beyond the name (possibly "Remote Request Protocol" — not confirmed) |

Each module's `-100-1-B02:BRF`/`-100-2-B02:BRF` pair is the familiar 1-bank/2-bank runtime split
documented in [TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md); the
`-500-B02:NRF` file is the ND-500-side relocatable object. `:DEFS` and `:IMPT` are almost
certainly PLANC `$INCLUDE`-style definition/import files for the module's call interface (not
opened/decoded further here).

**Installer**: `COS-LIB-IN-B02:PROG` — a real, compiled installer program, present on the floppy.
Its interactive prompts were not decoded (compiled binary, not a `:MODE`/`:BATC` script) — run it
directly on a live system to install.

## Installing on a machine (D100)

**Route A (the product installer) is the correct way and it WORKED on D100 on 2026-08-18
(full session log below).** Route B (hand copy with ndtool) is documented only as a
fallback; it landed files in the wrong user and left orphans - do not use it.

### Route A - the product installer (CONFIRMED WORKING 2026-08-18)

The installer `COS-LIB-IN-B02:PROG` copies ALL five module families (XMP, XMF, RRP, TLP, TLF)
to a destination user in one pass. It **defaults to user SYSTEM** and copies the whole set
there - the "module menu" and per-module choices did not appear in the real run; it just
copied everything.

Steps, exactly as they ran:

1. **Log in as SYSTEM.** This is mandatory - the installer checks it and aborts otherwise
   with `Illegal user name` / `*****> Log in as user SYSTEM to perform this installation.`
   (its first macro line is `^IF '<_USER>' .NEQS. 'SYSTEM' THEN ^ENTER SYSTEM`).
2. **Attach the floppy** to the running emulator (RetroCore console: `attach fd0
   10609B02-XX-01D.img`, `cont`; the D100 `RetroCore.ini` already has `device add FX 0`).
   **Operator action - agents must not drive or restart the emulator.**
3. **Enter the floppy directory** (`enter-dir ,, FLOPPY-DISC-1, 0`); verify with
   `@LIST-DIRECTORIES-ENTERED` - expect `... UNIT 0 : 10609B02-XX-01D`.
4. **Do NOT run the installer straight from the floppy path.** `@(10609B02-XX-01D:FLOPPY-USER)COS-LIB-IN-B02:PROG`
   prints the banner then dies in "Performing initial checks" with `"COS-LIB-IN-B02:PROG" /
   NO SUCH FILE NAME` - it is an XCOM macro engine that reopens files (including itself) **by
   bare name against your default directory**, and it refuses the floppy as default
   (*"Floppy directories can not be default directory"*). Instead **copy the installer onto
   SYSTEM and run it by bare name**:
   ```
   @COPY-FILE "COS-LIB-IN-B02:PROG",(10609B02-XX-01D:FLOPPY-USER)COS-LIB-IN-B02:PROG
   @COS-LIB-IN
   ```
   (`COPY-FILE <dest>,<source>` - the quoted destination CREATEs the new file.)
5. **Answer the one prompt: the destination user.** The installer asks *"If the files should
   be copied to user SYSTEM, type CR, otherwise type user name."* - **press CR to accept
   SYSTEM** (that is where the real run put everything: `(PACK-ONE:SYSTEM)`). It then copies
   all 24 module files; if a file already exists it asks *"Do you want to rewrite the file ?
   (Y/N)"* per file (only happens if something was pre-placed). It finishes with
   `*****> Directory 10609B02-XX-01D released.` and `*****> Installation completed.`

**Result - the library is installed on `(SYSTEM)`, not UTILITY.** So from PLANC:
`$INCLUDE XMP-B02:DEFS` / `$INCLUDE XMP-B02:IMPT` resolve bare (SYSTEM is the default
directory), and the BRF-LINKER load is `LOAD XMP-100-1-B02` (bare) or `LOAD (SYSTEM)XMP-100-1-B02`
- **not** `(UTILITY)`. Verify: `@BRF-LINKER-C01`, `LIST-BRF-ENTRIES XMP-100-1-B02,TERMINAL`
must list the `XMP*` entries (14 units / 55 entries - log:
[XMP-100-1-B02-LIST-BRF-ENTRIES-D100-2026-08-18.log](XMP-100-1-B02-LIST-BRF-ENTRIES-D100-2026-08-18.log)).

**Note on the destination-user default:** this installer defaults to **SYSTEM** (answer CR).
That differs from the sibling Backup-System installer INST-BASY-I04, whose prompt defaults to
UTILITY. Do not assume UTILITY for COSMOS-library - and in neither case answer `y` at a
user-name prompt (it is taken as a literal username).

### Route B - direct ndtool copy (fallback only; NOT recommended)

The earlier hand copy put files under `(UTILITY)` and `(SYSTEM)`, which is the WRONG layout -
the real installer uses SYSTEM only, so a Route-B UTILITY copy just leaves orphan files. If
you ever must do it (installer unavailable), copy to SYSTEM to match, and delete any stray
UTILITY copies afterwards. Requires the emulator STOPPED (image locked while it runs) - an
operator action. From Windows, with the extracted files in [files/](files/):

```
ndtool --put XMP-B02.DEFS      "UTILITY/XMP-B02:DEFS"      --overwrite BIGDISK0-K-100.IMG

Requires the emulator STOPPED (the image is locked while it runs) - an operator action.
From Windows, with the extracted files in [files/](files/):

```
ndtool --put XMP-B02.DEFS      "UTILITY/XMP-B02:DEFS"      --overwrite BIGDISK0-K-100.IMG
ndtool --put XMP-B02.IMPT      "UTILITY/XMP-B02:IMPT"      --overwrite BIGDISK0-K-100.IMG
ndtool --put XMP-100-1-B02.BRF "UTILITY/XMP-100-1-B02:BRF" --overwrite BIGDISK0-K-100.IMG
ndtool --put XMP-100-2-B02.BRF "UTILITY/XMP-100-2-B02:BRF" --overwrite BIGDISK0-K-100.IMG
ndtool --put XMP-500-B02.NRF   "UTILITY/XMP-500-B02:NRF"   --overwrite BIGDISK0-K-100.IMG
ndtool --chmod "PUBLIC+R" "UTILITY/<each file>" BIGDISK0-K-100.IMG
```

Text includes must be 7-bit CRLF before the put (strip the floppy's even-parity bit 7);
BRF/NRF go in raw and untouched. Take a backup copy of the image first.

## Using the library from PLANC

Full usage documentation - routine catalog, call conventions, the status model, which
include is authoritative for what, and the build steps:
[COSMOS-XMP-LIBRARY.md](../../../Developer/Languages/Application/COSMOS-XMP-LIBRARY.md).

## Documentation
- Product Information (PI-sheet): [../../Product-Info/ND-10609-A1-EN.md](../../Product-Info/ND-10609-A1-EN.md)

## Provenance
Real floppy image from the
[Norsk Data Software Archive](https://github.com/HackerCorpLabs/norskdata-software-archive)
(product `ND-10609`; local `/home/ronny/repos/norskdata-software-archive`), decoded with
`ndfs -t`/`ndtool -x`. There is no `:MODE`/`:BATC` install script on the floppy — only the
compiled installer `COS-LIB-IN-B02:PROG` — so the install procedure was recovered by decoding
that PROG (see "Installing on a machine" above), not transcribed from a script.

---
**Parent:** [../README.md](../README.md) (Software catalog)
