# Generic Software Install Methodology (SINTRAN III)

**A reusable procedure for installing Norsk Data application/subsystem software onto a running
SINTRAN III system.** Per-product specifics go in each version page; this is the shared pattern
they reference.

> **Sources & provenance:**
> - `[PD-EX]` verbatim worked example from the **ND-100 Pascal version J Installation** sheet
>   (`19840118_…Pascal_version_J_Installation`), read in full.
> - `[MODE]` re-entrant/RT-load patterns observed in real disk-image MODE files —
>   [../OS/research/HDD-IMAGE-FINDINGS.md](../OS/research/HDD-IMAGE-FINDINGS.md) §7.
> - `[PKG]` the ND "Software Library" 4-part release package —
>   [research/NDDOC-INVENTORY.md](research/NDDOC-INVENTORY.md) §5.
>
> Nothing here is invented. Where a product differs, follow ITS Installation sheet/MODE file,
> not this generic text. **Notation (do not over-read):** the source Installation sheet prints
> some commands with a leading **`§`** and others (inside the `§NRL` block) with a leading **`*`**.
> From the layout, `*` lines appear to be sub-commands entered inside the NRL loader and `§` lines
> appear to be top-level commands — but the exact meaning of the `§` glyph (and whether it maps to
> the standard `@` SINTRAN prompt) is **NOT verified** from the sources read. See §6 Open items.

---

## 1. Mental model

ND software ships on floppy as a small set of files — in the one example read in full (Pascal J),
this was:
- a compiled image in **`:BRF`** (the sheet says "BRF format"; the expansion of "BRF" is not
  stated in the sources read), e.g. `PASCAL-COD`, plus libraries
  (`PASCAL-LIB` / `PASCAL-2LIB`), and
- symbolic data such as error messages in **`:SYMB`**, and/or ready binaries in **`:BPUN`**.

Installing = **(a)** copy those files to system files with the correct type, **(b)** link/dump
them into a runnable `:PROG` and/or a **re-entrant subsystem** using the loader, and **(c)** for
resident services, hook them into the boot mode files so they come up automatically.

## 2. The release package (what you should have) `[PKG]`
| Part | Use during install |
|------|--------------------|
| Program Description (PD-sheet) | identifies files, ND-number, target CPU/OS — your checklist |
| **Installation** sheet | the authoritative step list (often branched per SINTRAN version) |
| Diskette manifest | confirms the floppy's file set / directory name |
| Revision Log | version history |

If the Installation sheet exists, **it wins** over this generic guide.

## 3. Tools used
- **NRL** — the loader invoked as `§NRL`; in the Pascal example it links `:BRF` files and dumps a
  `:PROG`. Its full name is **not confirmed** from the sources read (candidate: the ND Relocating
  Loader, `ND-60.066` — unverified). `[PD-EX]`
- **SINTRAN monitor re-entrant commands** — `§DUMP-REENTRANT`, `§DUMP-PROGRAM-REENTRANT`, `§DITAP`
  (names as printed in the Pascal sheet). `[PD-EX]`
- **RT-LOADER** — used for RT programs / drivers / daemons (`READ-BINARY`, `READ-PROGFILE`,
  segment commands) as seen in disk MODE files. `[MODE]`
- **SINTRAN-SERVICE-PROGRAM** — segment files, RT-common, write-protect, datafields. `[MODE]`

## 4. Generic steps

1. **Load the distribution floppy** and copy its files to **system files** with the right type
   (`:BRF`, `:SYMB`, `:PROG`, `:BPUN`). Place files under the conventional home user (subsystem
   `:BPUN` → `UTILITY`; language/tool `:BPUN` → `BPUN-FILES`; `:PROG` → `SYSTEM`) — see
   [HDD-IMAGE-FINDINGS](../OS/research/HDD-IMAGE-FINDINGS.md) §6. `[PD-EX][MODE]`
2. **Build the program with NRL** — set image file & size, load the BRF, read the entry/restart
   addresses, dump a `:PROG`:
   ```
   §NRL
   *IMAGE-FILE 100
   *SIZE 1500
   *LOAD <code-file> <library>
   *VALUE <symbol>            → prints an address value (xxxxxx)
   *VALUE CONTINU             → prints an address value (yyyyyy)
   *DUMP "<NAME>:PROG",xxxxxx,yyyyyy
   *EXIT
   ```
   (The two address values from `*VALUE` are passed to `*DUMP`; their exact roles — e.g. start
   vs restart — are not labelled in the source sheet.)
3. **Make it re-entrant.** In the Pascal example this step **branches on SINTRAN version**
   (whether every product branches this way is not established — follow the product's own sheet):
   - **SINTRAN H:** `§DITAP "<NAME>" <NAME>` then
     `§DUMP-REENTRANT <NAME>,xxxxxx,yyyyyy,<segname>`
   - **SINTRAN I or later:** `§DUMP-PROGRAM-REENTRANT <NAME>,<segname>`
   `[PD-EX]`
4. **(RT programs / drivers / daemons only)** load via RT-LOADER instead of/after NRL `[MODE]`:
   ```
   §RT-LOADER
   NEW-SEGMENT <seg> … / SET-PAGE-TABLE n
   READ-BINARY|READ-PROGFILE (<user>)<file>,<seg>,…
   DECLARE-PROGRAM <prog> / CHANGE-RT-DESCRIPTION <prog> <prio> <seg> …
   WRITE-SEGMENT <seg> / EXIT
   ```
5. **(Resident services only) make it start at boot** by editing the boot mode files `[MODE]`:
   - put the load/dump in a `*-LOAD:MODE` called from **HENT-MODE** (cold start), and
   - put the start command in a `*-START:MODE` called from **LOAD-MODE** (warm start),
   referenced as `@MODE (<user>)<file>:MODE,,,`. See
   [../OS/08-AUTOMATIC-BOOT-INITIAL-COMMANDS.md](../OS/08-AUTOMATIC-BOOT-INITIAL-COMMANDS.md).
6. **Verify** per the product's user manual.

## 5. Verbatim worked example — ND-100 Pascal version J `[PD-EX]`

Files: `PASCAL-COD` (compiler, BRF), `PASCAL-LIB`/`PASCAL-2LIB` (runtime, BRF; 2LIB = two-bank),
`PASCAL-ERR` (errors, symbolic).

```
# 1–2  copy distribution files to typed system files
   PASCAL-LIB  → PASCAL-LIB:BRF
   PASCAL-2LIB → PASCAL-2LIB:BRF
   PASCAL-ERR  → PASCAL-ERR:SYMB
# (compiler is a two-bank program; a terminal running it needs 128K user segments)

# 3  dump as re-entrant subsystem
SINTRAN version H:                    SINTRAN version I or later:
   §NRL                                  §NRL
   *IMAGE-FILE 100                       *IMAGE-FILE 100
   *SIZE 1500                            *SIZE 1500
(o)*DEFINE LINPP n                    (o)*DEFINE LINPP n
(o)*DEFINE NOBUF 4                    (o)*DEFINE NOBUF 4
   *LOAD PASCAL-COD PASCAL-2LIB          *LOAD PASCAL-COD PASCAL-2LIB
   *VALUE PASCAL      → xxxxxx           *VALUE PASCAL      → xxxxxx
   *VALUE CONTINU     → yyyyyy           *VALUE CONTINU     → yyyyyy
   *DUMP "PASCAL:PROG",xxxxxx,yyyyyy     *DUMP "PASCAL:PROG",xxxxxx,yyyyyy
   *EXIT                                 *EXIT
   §DITAP "PASCAL" PASCAL                §DUMP-PROGRAM-REENTRANT PASCAL,PASCAL
   §DUMP-REENTRANT PASCAL,xxxxxx,yyyyyy,PASCAL
```
"The installation is now complete, and the system can be used according to the user manual."
`(o)` = optional (`*DEFINE LINPP n` = lines/page, octal; `*DEFINE NOBUF 4` = files buffered).

> This example is the canonical shape: **copy typed files → NRL load+dump → version-branched
> re-entrant dump.** It also confirms the H-vs-I+ branch seen in the disk-image MODE files.

## 6. Open items
- Confirm the exact meaning of `§` vs `@` prompt rendering and `§DITAP` semantics against the
  System Supervisor / Relocating Loader manuals (folder 30 / `ND-60.066`).
- Add an RT-driver worked example (e.g. an XMSG or COSMOS load) once transcribed verbatim.

---
**Parent:** [README.md](README.md) · templates: [_templates/](_templates/)
