# ND-10311 — Assembler for ND-500

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10311` |
| Product name | ND-500 Assembler |
| Functional category | Language Tools — Linkers / Loaders / Debuggers / Assemblers / Monitors |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 |
| OS requirement | unknown — no PD sheet located |
| Related products | `ND-10335` ND-500 Symbolic Debugger (this catalog documents its built-in assembler/disassembler as a debugging aid — separate product from the standalone assembler here); `ND-10319` ND-500 Linkage-Loader |

## Description
The ND-500 assembler: `MODULE`/`ENDMODULE`, `IMPORT-P`/`IMPORT-D`/`EXPORT`, `ROUTINE`/`ENDROUTINE`,
macros, conditional assembly (`$IF`/`$ELSIF`/`$ELSE`/`$ENDIF`), and an `ASSEMBLE <source> <list>
<object>` command producing `:SYMB`/`:LIST`/`:NRF` by default. Full command/directive reference:
[../../Product-Info/ND-10311-A1-EN.md](../../Product-Info/ND-10311-A1-EN.md). [PI]

**No install-procedure PD sheet has been located** — but the install command itself is already
documented elsewhere in this repo, quoted verbatim from the SINTRAN III System Supervisor manual.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A | [ND-10311A](ND-10311A/README.md) | IN-PROGRESS — install command sourced from the manual, not this product's own PD sheet | floppy `ND-10311A` |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-10311-A1-EN.md](../../Product-Info/ND-10311-A1-EN.md)
- Manual(s): `ND-60.113` NORD-500 Assembler Reference Manual — [../../../Reference-Manuals/ND-60.113.02 EN Assembler Reference Manual.md](../../../Reference-Manuals/ND-60.113.02%20EN%20Assembler%20Reference%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading the image (MD5 `ec1a89cbd51ada86af596609d9bf40e3`) and
reading it with `ndtool`. Install command: `ND-30.003.7 EN SINTRAN III System Supervisor`, quoted
in [../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md §12](../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md).

---
**Parent:** [../README.md](../README.md) (Software catalog)
