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

## Documentation
- No PD sheet, no PI sheet, no ND article number located.

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x` for listing/extraction, `byte & 0x7F` for `SCREEN.SYMB`,
  `DEMO-SCREEN.SYMB`'s header, and `SUM.PICT`, all in full).
- **TODO:** `PLANC-GEN-A00:PROG` (likely a picture-file generator/editor, by analogy to NSHS's
  Screen Picture Maintenance Program) is a compiled binary, not decoded. `SUM:SYMB` (the PLANC
  source using this interface) was not opened. The relationship to `ND-10013` NSHS remains
  unconfirmed — worth resolving if an NSHS manual surfaces (the PI sheet alone doesn't show the
  `.PICT` file syntax to compare against).

---
**Parent:** [../README.md](../README.md) (Software catalog)
