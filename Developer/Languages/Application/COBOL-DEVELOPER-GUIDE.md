# COBOL Developer Guide

**COBOL Programming Language for SINTRAN III**

**Version:** 1.0 (Placeholder)  
**Date:** October 18, 2025  
**Status:** Reference Guide

---

## Installing COBOL

ND-500 COBOL only — no PD sheet exists for either article number, so everything below comes from
the floppies themselves:

- **Older article** — [ND-10177](../../../Installation/Software/ND-10177/README.md) (product
  `ND-10177`), version
  [ND-10177H00](../../../Installation/Software/ND-10177/ND-10177H00/README.md) — a 3-disk set
  with the middle disk (`10177H00-2S`, presumably the runtime library) missing from every source
  checked.
- **Later article (COBOL-85)** — [ND-210177](../../../Installation/Software/ND-210177/README.md)
  (product `ND-210177`), two versions found:
  [ND-210177J02](../../../Installation/Software/ND-210177/ND-210177J02/README.md) (real
  `Linkage-Loader` `:MODE` scripts decoded straight from the floppy — the most verified of the
  two) and
  [ND-210177K01](../../../Installation/Software/ND-210177/ND-210177K01/README.md) (no script on
  this disk, procedure adapted from J02, unverified).

Both articles ship as ND-500 **domains** loaded via the Linkage-Loader, not plain SINTRAN
reentrant subsystems — install the [ND-500 Linkage-Loader](../../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
first if it isn't already present. Both articles also bundle an ISAM (indexed-file) add-on,
corresponding to the separate product `ND-10343`.

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md) Section 6 for complete Hello World example.**

### Minimal Example

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.

PROCEDURE DIVISION.
    DISPLAY 'HELLO FROM COBOL!'.
    STOP RUN.
```

### Build Process

```bash
@COBOL HELLO:COB            # Compile COBOL to BRF
@NRL                        # Start linker
*PROG-FILE "HELLO"
*LOAD HELLO
*EXIT
@HELLO                      # Run
```

---

## Reference Manual

**Complete COBOL language documentation:**

**Location:** [Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md](../../../Reference-Manuals/ND-60.144.3%20EN%20COBOL%20Reference%20Manual.md)

**Manual:** ND-60.144.3 EN COBOL Reference Manual

---

## Key Features

1. **ANSI COBOL Standard:** ANSI-74 COBOL compatible
2. **Business Applications:** Designed for business data processing
3. **SINTRAN Integration:** Full access to SINTRAN III facilities
4. **Compiler:** `@COBOL` for ND-100

---

## See Also

- **[QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Linking COBOL programs
- **[SINTRAN-DEVELOPER-GUIDE.md](../../SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **Reference Manual:** [ND-60.144.3 EN COBOL Reference Manual](../../../Reference-Manuals/ND-60.144.3%20EN%20COBOL%20Reference%20Manual.md)

---

**For comprehensive COBOL development, refer to the reference manual and QUICK-START-EXAMPLES.md.**


