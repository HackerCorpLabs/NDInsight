# SINTRAN File Formats

**Byte-level documentation of the Norsk Data object and executable file formats.**

---

## Files

| File | Contents |
|------|----------|
| [BRF-FILE-FORMAT.md](BRF-FILE-FORMAT.md) | **BRF (Binary Relocatable Format)** - the relocatable object-code format produced by all ND language processors (MAC, FORTRAN, COBOL, PLANC, BASIC, PASCAL, NPL, C) and consumed by the Relocating Loader, the Real Time Loader and the BRF Editor |
| [BRF-GHIDRA-LOADER-HANDOFF.md](BRF-GHIDRA-LOADER-HANDOFF.md) | Handoff document for building a Ghidra `Loader` for BRF files (memory layout, symbols, entry points, relocations for ND-100 code). No loader exists yet; a validated decoder does. |
| [PROG-FILE-FORMAT.md](PROG-FILE-FORMAT.md) | **`:PROG` format** - comprehensive description of the SINTRAN III program file format used by the Nord Relocating Loader (NRL), `@DUMP` / `@RECOVER`, and (extended) by the ND-500 Loader Monitor |
| [prog-fileformat.md](prog-fileformat.md) | `:PROG` on-disk byte-level layout (companion to the reference above) |

---

## Related

- The BPUN (Bootable Punched Tape) container format is documented in
  [../../Installation/Communication/Ethernet/x/stripped/README.md](../../Installation/Communication/Ethernet/x/stripped/README.md)
  (verified field-by-field against the ENCOS firmware files, including the
  word-count-0 = 65536 quirk).

---

**Parent:** [../README.md](../README.md)
