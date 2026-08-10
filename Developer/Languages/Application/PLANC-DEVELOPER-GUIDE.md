# PLANC Developer Guide

**PLANC Programming Language for SINTRAN III**

**Version:** 2.0  
**Date:** May 26, 2026  
**Status:** Verified on ND-100 hardware

---

## Installing PLANC

PLANC for ND-100 is product `ND-10309`. Three versions are catalogued — pick whichever floppy you
have:

- [ND-10309A](../../../Installation/Software/ND-10309/ND-10309A/README.md) (82.02.19) and
  [ND-10309B](../../../Installation/Software/ND-10309/ND-10309B/README.md) (82.06.15) — verified
  from their PD sheets, ship as `:BPUN` (`@DUMP-REENTRANT PLANC-100,0,1,<file>`).
- [ND-10309F](../../../Installation/Software/ND-10309/ND-10309F/README.md) (floppy
  `10309F00-1S`) — no PD sheet located; ships pre-linked as `:PROG`, procedure adapted from A/B
  and not independently verified.

All versions need a 126K/128K-word terminal background segment
(`@CHANGE-BACKGROUND-SEGMENT-SIZE`) and cannot be restarted with `@CONTINUE`. See
[ND-10309 — PLANC for ND-100](../../../Installation/Software/ND-10309/README.md) for the full
product overview.

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md) Section 4 for complete Hello World example.**

### Minimal Example

**File:** `HELLO:PLNC` (must have CRLF line endings and even parity)

Source files use type `:PLNC` or `:SYMB`. The compiler looks for `:SYMB` first, then `:PLNC`.

```planc
MODULE hello
    INTEGER ARRAY : stack(0:100)
    BYTES : msg := 'HELLO FROM PLANC!'

    PROGRAM : main
        INISTACK stack
        OUTPUT (1,'AL17',msg)
        OUTPUT (1,'AL1','$')
    ENDROUTINE
ENDMODULE
```

### Build Process

**Interactive build (verified on ND-100, PLANC Version E):**

```
@PLANC
PROG-FILE "HELLO"
COMPILE HELLO:PLNC,"HELLO:LIST","HELLO"
EXIT
@HELLO
```

**Automated build (MODE file):**

File: `DO-BUILD:MODE`

```mode
@DELETE-FILE HELLO:PROG
@DELETE-FILE HELLO:LIST
@DELETE-FILE HELLO:BRF
@PLANC
PROG-FILE "HELLO"
COMPILE HELLO:PLNC,"HELLO:LIST","HELLO"
EXIT
@HELLO
```

Run with: `@MODE DO-BUILD:MODE,,`

See also [BUILD-PLANC.MODE](../../Workflow/BUILD-PLANC.MODE) and [BUILD-PLANC-2BANK.MODE](../../Workflow/BUILD-PLANC-2BANK.MODE) for parameterized builds.

### Build Output

The compiler produces three files:
- `HELLO:PROG` -- executable program (run with `@HELLO`)
- `HELLO:BRF` -- binary relocatable file
- `HELLO:LIST` -- compiler listing with line numbers

### COMPILE Parameters

`COMPILE source, list, object`

1. `source` -- source file (exists, no quotes needed)
2. `list` -- listing output (created, needs `"quotes"`)
3. `object` -- object file name (created, needs `"quotes"`, produces :BRF)

**SINTRAN quoting rule**: files being CREATED need `"quotes"`. Existing files do not.

---

## PLANC Syntax Summary

- `MODULE`/`ENDMODULE` structure -- NOT Pascal's PROGRAM/BEGIN/END
- Entry point: `PROGRAM : name` inside the module
- `INISTACK` must be called first to initialize the runtime stack
- `OUTPUT(device, format, variable)` writes to terminal (device 1)
- `'AL17'` = Alphanumeric, Left-justified, 17 characters
- `'$'` in output strings = CR+LF (newline). Literal `$` = `$$`
- `BYTES : name := 'string'` declares byte array with implicit length
- `%` starts a comment to end of line
- `&` at end of line continues statement on next line
- `;` separates multiple statements on one line
- Assignment: `expression =: variable` (NOT `:=`)
- Not-equal: `><` (NOT `!=` or `<>`)
- All SINTRAN text files require CRLF line endings and even parity (bit 7)

---

## Reference Manual

**Complete PLANC language documentation:**

**Location:** [Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md](../../../Reference-Manuals/ND-60.117.5%20EN%20PLANC%20Reference%20Manual.md)

**Manual:** ND-60.117.5 EN PLANC Reference Manual (5th Ed., March 1986, Version G)

---

## Key Features

1. **MODULE-based:** Programs structured as modules with EXPORT/IMPORT
2. **Strong Typing:** Type-safe development with records, sets, pointers
3. **SINTRAN Integration:** Full monitor call access via Monitor_Call()
4. **Compiler:** `@PLANC` on ND-100, `@PLANC-500` on ND-500
5. **XMSG IPC:** Via COSMOS XMP library routines (MON 200B not directly available)
6. **Inline Assembly:** `$*` prefix for MAC assembler statements

---

## See Also

- **[QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Linking PLANC programs
- **[SINTRAN-DEVELOPER-GUIDE.md](../../SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **[SCRIPT-GUIDE.md](../../Workflow/SCRIPT-GUIDE.md)** - MODE file automation
- **Reference Manual:** [ND-60.117.5 EN PLANC Reference Manual](../../../Reference-Manuals/ND-60.117.5%20EN%20PLANC%20Reference%20Manual.md)
- **Monitor Calls:** [ND-860228-2 SINTRAN III Monitor Calls](../../../Reference-Manuals/ND-860228-2-EN%20SINTRAN%20III%20Monitor%20Calls.md)

---

**Last Updated:** May 26, 2026  
**Version:** 2.0
