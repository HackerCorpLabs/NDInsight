# ND-211159 — LED-FORTRAN

> Status: STUB — real floppy found and decoded, but no PD/PI sheet and no installer program on the floppy

| Field | Value |
|-------|-------|
| ND article number | `ND-211159` |
| Product name | LED-FORTRAN (language-aware LED editor mode for FORTRAN — per the background sweep's findings) |
| Functional category | Editors |
| Related products | `ND-211160` LED for ND-500/5000 — see [../ND-211160/README.md](../ND-211160/README.md) · `ND-211157` LED-DEBUGGER — see [../ND-211157/README.md](../ND-211157/README.md) · `ND-211158` LED-PLANC — see [../ND-211158/README.md](../ND-211158/README.md) |

## What is known

No PD sheet and no PI sheet exist for this article number in this repo's archive. One floppy
does exist and was downloaded and decoded:

| Floppy volume | MD5 | Contents |
|---|---|---|
| `211159A01-XX-01D` | `16f5eeaf98e0cdda5114ca0525fe5022` | dated 1987-05-26. `DESCRIPTION-FILE:DESC` (22528 bytes) · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholders) · `LED-FORTRAN-A01:LINK`(13057 bytes)/`:DSEG`(1017597 bytes)/`:PSEG`(267382 bytes) |

This is the same three-file *domain* shape (`:LINK`/`:DSEG`/`:PSEG`) used throughout this catalog
for a ND-500 Linkage-Loader-format domain (e.g. `ND-211037`'s `PLANC-100-I01`) — so
`LED-FORTRAN-A01` is a compiler/editor-extension domain meant to be `COPY-DOMAIN`'d onto the
target system, same as the PLANC-family cross-compilers.

**`DESCRIPTION-FILE:DESC` was decoded** (`byte & 0x7F`) — it is **not** free install text. It is
the same binary domain-description structure the Linkage-Loader itself reads (domain names,
segment sizes, floppy volume-name back-reference `211159A01-XX-01D:FLOPPY-USER`) — i.e. metadata
for the loader, not a human-readable install procedure. **There is no `:PROG`/`:XCOM`/`:INIT`
installer on this floppy** — unlike every other installer-driven product in this catalog, so an
install procedure cannot be derived from the floppy alone.

## Provenance
Floppy image downloaded and decoded (file listing + `DESCRIPTION-FILE:DESC` byte-level dump); no
install procedure could be recovered from it. No PD sheet or PI sheet located. This is the one
sweep-list item with a real floppy but genuinely nothing to transcribe as an install procedure —
do not invent a `COPY-DOMAIN` sequence for it without a manual confirming the target domain name.

---
**Parent:** [../README.md](../README.md) (Software catalog)
