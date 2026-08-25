# PLANC-SCREEN-H — PLANC Screen Handling Library (no confirmed ND article number)

> Status: IN-PROGRESS — real floppy decoded, real PLANC interface + real `.PICT` screen-picture format recovered; no ND article number found

| Field | Value |
|-------|-------|
| ND article number | **not identified** — floppy volume name is simply `PLANC-SCREEN-H`, no ND-XXXXX number found anywhere on it |
| Product name | PLANC screen-handling library (working name only) |
| Functional category | Language Tools / UI building |
| CPU target | ND-100 (PLANC) |
| Related products | A **third**, real, concrete answer to "how do I build a UI on ND hardware" alongside VTM ([VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md)) and NSHS ([ND-10013](../ND-10013/README.md)) — this floppy's `%HEADING`/`@position`/`@size`/`@field-defaults` `.PICT` file format looks like the same "screen picture" concept NSHS's PI sheet describes, strongly suggesting this **is** NSHS's PLANC interface library, not a separate product — not confirmed either way. |

## What is known — real floppy, decoded

Floppy `PLANC-SCREEN-H` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen, dated
1986-1987 per file timestamps) mounts cleanly:

```
DEMO-SCREEN:SYMB       real PLANC demo source — see below
PLANC-GEN-A00:PROG     a compiled "PLANC-GEN" program (generator? not decoded further)
INTERF:NRF             the compiled interface library
SUM:PICT               a real screen-picture definition file — see below
SUM:SYMB               PLANC source for the SUM demo (not opened)
SCREEN:SYMB            the real PLANC IMPORT/interface declaration — see below
INTERF-1B:BRF          1-bank runtime (see TWO-BANK-PROGRAMS.md)
INTERF-2B:BRF          2-bank runtime
```

## The real PLANC call interface — `SCREEN:SYMB`, verbatim

Source: byte-for-byte decode (`byte & 0x7F`), in full — 926 bytes. [decoded from real file]

```
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,BYTES,BYTES):bytdis)
IMPORT (ROUTINE VOID,BYTES(INTEGER,INTEGER,INTEGER,BYTES READ WRITE,BYTES):bytacc)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER4,BYTES):intdis)
IMPORT (ROUTINE VOID,BYTES(INTEGER,INTEGER,INTEGER,INTEGER4 READ WRITE,BYTES):intacc)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,BYTE,INTEGER,INTEGER,REAL8,BYTES):realdis)
IMPORT (ROUTINE VOID,BYTES(INTEGER,INTEGER,INTEGER,REAL8 READ WRITE,BYTES):realacc)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER,BYTES):frame)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):fullbar)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):sparsebar)
IMPORT (ROUTINE VOID,VOID:blankscreen)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):blankarea)
IMPORT (ROUTINE VOID,VOID:resetscreen)
```

**Reading it**: ten real callable routines. `bytdis`/`bytacc`, `intdis`/`intacc`, `realdis`/
`realacc` are display/access pairs for byte-string, integer, and real fields respectively (`dis`
= display a field's current value read-only, `acc` = access — display and let the user edit,
returning the new value). `frame`/`fullbar`/`sparsebar` draw box frames and progress-bar-style
fills. `blankscreen`/`blankarea`/`resetscreen` are screen-clearing primitives.

## A real demo program — `DEMO-SCREEN:SYMB`, header comment verbatim

```
MODULE xxx
%==========================================================%
% D E M O   -   PLANC SCREEN HANDLING                      %
%                                                          %
% Vtm, mon-call-lib,planc-lib must be loaded               %
% together with this program                               %
%==========================================================%
$INCLUDE screen
```

**Confirms this library is built on top of VTM** (`Vtm... must be loaded together with this
program`) — consistent with every other UI-building path documented in this catalog (VTM is the
common low-level substrate; NSHS, UNIQUE, and this PLANC library are all higher-level layers over
it).

## The real screen-picture file format — `SUM:PICT`, verbatim

Source: byte-for-byte decode, in full. [decoded from real file]

```
%HEADING
@picture sum-demo,i
%CONTROL
@position 1,35
@size 8,13
@in-frame heading remarks
@field-defaults prompt
%DEFINITIONS
@start

 *** SUM ***

 A   :  $$$$ @1
 B   :  $$$$ @2

 SUM : $$$$$ @3

@end
%ATTRIBUTES
@1 a underline
@2 b underline control add
@+    DISPLAY_FIELDS
@3 sum o low-intensity not-prompt
```

**Reading it**: a `.PICT` file has four `%`-prefixed sections. `%HEADING` names the picture
(`@picture sum-demo,i` — `i` likely a version/type letter). `%CONTROL` sets screen position/size
(`@position row,col`, `@size height,width`) and framing options (`@in-frame`, `@field-defaults`).
`%DEFINITIONS` is the actual screen layout between `@start`/`@end` — a fixed-format field with
`$$$$` placeholders, each tagged `@N` to link it to an `%ATTRIBUTES` entry. `%ATTRIBUTES` maps
each `@N` field to a program variable name (`a`, `b`, `sum`) plus display attributes
(`underline`, `low-intensity`, `not-prompt` = display-only) and control flags (`control add` —
likely marks this field as one the `DISPLAY_FIELDS` group updates automatically, e.g. a computed
sum). This is a genuinely different (and more compact) picture DSL than either VTM's raw API or
UNIQUE's `start-form`/`start-fields` convention — a real fourth documented UI-definition syntax
for this catalog.

## Installation procedure — INFERRED, NOT VERIFIED

**No PD/PI sheet, no `:MODE`/`:BATC` install script exists for this product.** Nothing below is
sourced from an install document — it is reconstructed from the floppy's own file layout, the
demo program's header comment, and the verified installation pattern of the closest documented
sibling product, [`ND-211464`](../ND-211464/ND-211464A/README.md#installation-procedure). Treat
every step as a working hypothesis to test on a real system, not a transcribed procedure.

**1. Prerequisites — install these first, separately:**
- **VTM terminal tables** for your target terminal(s) — see
  [`ND-211464`](../ND-211464/README.md) (verified procedure), [`ND-210455`](../ND-210455/README.md),
  [`ND-10459`](../ND-10459/README.md), or [`ND-10465`](../ND-10465/README.md) depending on
  terminal model. This product's floppy carries no VTM tables of its own — unlike
  [FOCUS](../ND-10188/README.md), which bundles its own.
- **`mon-call-lib`** — product [`ND-210913`](../ND-210913/README.md). Install procedure now
  verified (transcribed from NDWiki; primary PD-sheet PDF still pending) — copy
  `MON-CALL-NAMES-A:DATA` to `SYSTEM`, the other three files anywhere with public read access.
- **`planc-lib`** — not a separate product to install; treated in this catalog as a resident
  system library already present under `(LIBRARIES)` wherever PLANC itself is installed (see
  [PLANC-UI-VTM-GUIDE.md §4](../../../Developer/Languages/Application/PLANC-UI-VTM-GUIDE.md#4-building-and-linking-a-screen-program)
  for the full reasoning) — inferred, not confirmed by a manual.
- The base **PLANC compiler**, [`ND-10309`](../ND-10309/README.md).

**2. Copy the floppy's files** to `SYSTEM` or your own working user — the same plain-copy
approach every small library floppy in this catalog uses when no installer program exists (e.g.
`ND-210913`'s `mon-call-lib` floppy). No installer `:PROG`/`:MODE`/`:BATC` was found on this
floppy to run instead.

**3. Pick the runtime matching your program's bank model** — `INTERF-1B:BRF` (1-bank) or
`INTERF-2B:BRF` (2-bank), see [TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md).
(`INTERF:NRF` is the ND-500 equivalent, format-consistent with the ND-100 choice being
per-program rather than per-installation.)

**4. Compile your program with `$INCLUDE screen`** (the interface declared in `SCREEN:SYMB`),
then link it against the chosen `INTERF-*B:BRF`, your VTM array file, `mon-call-lib`, and
`planc-lib` — see [PLANC-UI-VTM-GUIDE.md §4](../../../Developer/Languages/Application/PLANC-UI-VTM-GUIDE.md#4-building-and-linking-a-screen-program)
for the concrete `LOAD-SEGMENT`/`NRL` shape (also inferred, not a verified transcript).

**5. If using a `.PICT`-declared screen**, run it through `PLANC-GEN-A00:PROG` first to produce a
`:PGEN` PLANC source file, then `$INCLUDE` that generated file and call the picture's
name-derived routine directly — see
[PLANC-UI-VTM-GUIDE.md §6](../../../Developer/Languages/Application/PLANC-UI-VTM-GUIDE.md#6-how-a-pict-file-actually-gets-used--symsymb-and-planc-gen-a00prog)
for the fully decoded example of this step.

## Documentation
- No PD sheet, no PI sheet, no ND article number located.

## Provenance & open items
- Source: one real floppy image (`8_nd_f17b_planc-screen-h.img.gz`), mounted and decoded in this
  session (`ndtool -x -p` for extraction, `byte & 0x7F` for de-parity on `SCREEN:SYMB`,
  `DEMO-SCREEN:SYMB`'s header, `SUM:PICT`, and `SUM:SYMB`, all in full).
- `PLANC-GEN-A00:PROG` is identified by strings (a real `.PICT`-to-PLANC-source generator,
  confirmed by strings referencing `@PICTURE`/`@REPORT` input and emitting `FRAME(...)` calls) but
  not disassembled — its exact command-line dialogue is unknown.
- The relationship to `ND-10013` NSHS, and now also to `ND-10188` FOCUS, remains unconfirmed —
  see the full comparison in
  [PLANC-VTM-UI-CATALOG.md §8](../../../Developer/Workflow/PLANC-VTM-UI-CATALOG.md#8-what-is-still-open-across-this-whole-catalog).
- The install procedure above has **not been run live** on any real or emulated SINTRAN system.

---
**Parent:** [../README.md](../README.md) (Software catalog)
