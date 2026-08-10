# CAT-CAT5-B06 - CAT compiler code-generation back-end (used by NC)

## Overview

CAT-CAT5-B06 is the Norsk Data CAT compiler code-generation back-end
(the "CAT_COMPILER"), version B06, 1988. [verified]

It is NOT normally run directly by a user. It is the code generator invoked BY
the NC-A06 C compiler during its GENERATE-CODE step: NC nests it through the
SINTRAN monitor call MON 317B (UECOM). When it finishes it prints
`program CAT_COMPILER terminated`. [verified]

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `CAT-CAT5-B06.DOM` - the runnable ND-500 domain (the back-end itself).
  [verified]

The `analysis/` folder is empty - no disassembly or RE notes are present for
this program. [verified]

## Requirements

- The `.DOM` file. [verified]
- The CAT run-time library `CAT-LIB` (shared), at
  [../_shared/files/CAT-LIB.NRF](../_shared/files/CAT-LIB.NRF). [from disasm]
- Because it is driven by NC, the full C toolchain requirements apply: NC's
  libraries and the C linker auto-job (`NC-LIB`, `CAT-LIB`, `USLIB3`,
  `LINKER-AUTO-C.JOB`), all in [../_shared/files/](../_shared/files/). See
  [../README.md](../README.md) "Requirements model".
- Install: copy `files/CAT-CAT5-B06.DOM` and the shared libraries into the
  sintran-root. See [../README.md](../README.md).

## How to run

Normally you do NOT run this directly - you run the NC C compiler, which invokes
CAT as its back-end. See the NC-A06 userguide for the C compile chain.

Indirect (normal) use, via NC, scripted from `~/repos/nd500x`:

```
printf 'LOGIN GUEST\nNC-A06\nCOMPILE HELLO\nGENERATE-CODE\nEXIT\n' | \
    ./build/bin/nd500x --monitor --user GUEST --sintran-root ~/ND500USERS
```

(NC's GENERATE-CODE step nests CAT-CAT5-B06 through MON 317B UECOM.) [verified]

Direct invocation by typing the bare name `CAT-CAT5-B06` at the `@` prompt is
possible in principle but is not the intended interface; the program expects to
be driven by NC with the intermediate files NC produces. [UNVERIFIED direct use]

## Commands and options

Not user-facing. CAT-CAT5-B06 has no documented interactive command set of its
own - it takes its input (the intermediate representation) and control
parameters from NC through the nested UECOM invocation. [verified]

No `.HELP` file ships and `analysis/` is empty, so no command/option list could
be extracted. [verified]

## Verified behaviour in nd500x

Verified 2026-07-31 in the `nd500x` C emulator: CAT-CAT5-B06 runs when nested by
NC's GENERATE-CODE step and prints `program CAT_COMPILER terminated` on
completion. [verified]

## Known issues / status

- Runs as the NC back-end (driven indirectly). [verified]
- No standalone command interface is documented; treat it as an internal
  component of the C toolchain, not a user tool. [verified]
- No disassembly/RE notes available in `analysis/`. [verified]

## References

- Shared conventions: [../README.md](../README.md)
- Shared CAT library: [../_shared/files/CAT-LIB.NRF](../_shared/files/CAT-LIB.NRF)
- NC C compiler (the caller): [../NC-A06/userguide.md](../NC-A06/userguide.md)
- Runnable domain: [files/CAT-CAT5-B06.DOM](files/CAT-CAT5-B06.DOM)
