# ND-210190K02 — FORTRAN for ND-500, version K02

> Status: IN-PROGRESS — installer program identified and partially analysed, exact live dialogue NOT captured   ·   Install source: [OBS] (directory listing) + [INF] (binary string analysis)

| Field | Value |
|-------|-------|
| Part number | `210190K02` |
| Base product | [`ND-210190`](../README.md) |
| Version | K02 |
| Release date | files dated 1987-07-30 through 1987-12-12 |
| CPU target | ND-500 |
| OS requirement | SINTRAN III |

## Description
Native ND-500 FORTRAN compiler, shipped as an ND-500 **domain** (`:LINK`/`:DSEG`/`:PSEG` segment
triple) with a real installer program — not a plain-file, manual-copy product like every ND-100
FORTRAN floppy in this catalog.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210190K02-XX-01D` | `DESCRIPTION-FILE:DESC` (domain description, 11 pages) · `FORTRAN-500-K02:LINK`/`:DSEG` (47 pages)/`:PSEG` (152 pages) — the compiler domain · `FORTRAN-LIB-K02:NRF` (51 pages), `FORTRAN-TPS-K02:NRF` (53 pages) — runtime library and an optional "TPS" component · `IN-FORT-XX-K02:PROG` (66 pages, the installer) / `:INIT` (11 pages) / `:RSRC` (11 pages) — installer program + its data files · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` and `FIL-1-K02:PSEG`/`FIL-2-K02:PSEG` — empty placeholder segments, user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `7cde7e416ca1ec59c9698dc06896d4a0`) and reading with
`ndtool -t`/`ndtool -x`.

## What the installer binary reveals (recovered without a live run)

`IN-FORT-XX-K02:PROG` is a compiled program, not a plain-text `:MODE` script, so it cannot be
decoded the simple bit-masking way that worked for the CC-100 `:MODE` files. Its embedded string
constants were extracted directly, though, and they show enough to characterize it with
confidence, even without running it:

- It follows the **same module family as the ND-500 Linkage-Loader installer** already documented
  in [../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md §4](../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
  — embedded labels `^get_environment:` / `^check_environment:` and conditional branches keyed on
  a `<prog_part>` variable (`'1'` → get environment, `'3'` → check environment) mirror that
  installer's "Get start information" / "Check environment and resources" modules.
  `[INF — structural analogy, not a byte-for-byte match]`
- It **creates and sizes users**: literal command templates for `@CREATE-USER <<P0>>`,
  `@GIVE-USER-SPACE <<P1>>:<<P0>> <<P2>>`, `@DELETE-USER`, `@DELETE-USER-FILE`, and
  `@LIST-USER(S)` are all present as string templates with `<P0>`/`<P1>`/`<P2>` substitution
  placeholders — this is an XCOM-style command-file generator, the same kind of macro engine seen
  driving other ND installers.
- It has an **optional component**, controlled by an `<install_tps>` flag: when true, it copies
  `FORTRAN-TPS-<ver><rev>:NRF` (and otherwise `FORTRAN-LIB-<ver><rev>:NRF`) — i.e. the installer
  asks whether to install the TPS-related library variant, matching the two `:NRF` files seen on
  the floppy. What "TPS" stands for in this context (Transaction Processing System? Test Program
  System?) is **not determined** from the strings available.
- It manages **previous versions**: `RELEASE-DOMAIN`/`DELETE-DOMAIN <prev_dom_<file_nr>>` and
  `@DELETE-FILE <prev_file_<file_nr>>` templates exist, consistent with a "delete old version"
  module like the NLL installer's module 2.
- Some strings carry **Norsk Data's own internal build artifacts**: the path
  `(PACK-THREE-6248:VAN-THUAN)IN-FORT-XX-500:PROG` and Norwegian-language error text
  (`"$Doing ESC ESC !!$"` alongside `"$COM startet p} nytt.$"` — `p}` is a mojibake `på`) are
  leftovers from the compiler/installer's own build environment, not install-time output.

No prompt text resembling "Please specify..." or "(Y/N)" (the phrasing seen live in the NLL and
Backup-System installers) was found in the extracted strings — either such prompts are
constructed dynamically from smaller fragments (plausible, given the macro-template style seen
above) or a live run is genuinely required to see them.

## Installation procedure

**Not confirmed.** Given the strong structural resemblance to the NLL installer, the safe
starting point is the same general flow documented in
[../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md §4](../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md):

```
@(210190K02-XX-01D:FLOPPY-USER)IN-FORT-XX-K02:PROG
```
then expect a multi-module menu (get environment info → check environment → copy → exit), likely
asking for a domain-owning user and whether to install the `FORTRAN-TPS` component. **This has
not been run** — do not treat it as a confirmed procedure. If it follows the NLL pattern closely,
the same gotchas likely apply (a missing default user aborting the whole installer, a silent
domain-copy failure requiring `@LIST-FILES` verification afterward, etc.) — see
[../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md §5](../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
for the full gotcha list from that (different, but structurally similar) installer.

## Configuration / post-install
Unknown — likely a `DEFINE-STANDARD-DOMAIN`-style step for cold-start persistence, by analogy
with the NLL installer, but not confirmed.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-210190-B1-EN.md](../../../Product-Info/ND-210190-B1-EN.md)
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual

## Provenance & open items
- Source: floppy directory listing via `ndtool -t`; installer binary string constants extracted
  via `ndtool -x` + a raw ASCII scan (not disassembly) of `IN-FORT-XX-K02.PROG`.
- **TODO (blocking):** run the installer live (RetroCore emulator or a real machine) to capture
  its actual prompts, module structure, and any gotchas — everything above the "what the binary
  reveals" section is inference from static strings, not observed behavior.
- **TODO:** determine what "TPS" means in `FORTRAN-TPS-K02:NRF`.

---
**Parent:** [../README.md](../README.md) (`ND-210190` product overview)
